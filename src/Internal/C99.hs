{-# LANGUAGE AllowAmbiguousTypes #-}

module Internal.C99 where

import Internal.TypeRepr         (TypeRepr(..))
import Internal.Maps             hiding (Ident)

import Language.C99.Simple
import Data.Map                  (Map, fromList, findWithDefault)
import Control.Monad.State.Lazy  (State, get, put)
import Prelude                   hiding (LT)

{-

  Data types and auxiliar functions for C99 translation

-}

-- || Translation to C99 types

toC99Type :: TypeRepr -> Type
toC99Type TInt            = TypeSpec Int
toC99Type TDouble         = TypeSpec Double
toC99Type TBool           = TypeSpec Bool
toC99Type TChar           = TypeSpec Char
toC99Type p@(TMaybe t)    = (TypeSpec . TypedefName . show) p
toC99Type p@(TEither t u) = (TypeSpec . TypedefName . show) p
toC99Type p@(TList t)     = (TypeSpec . TypedefName . show) p
toC99Type p@(TProd t u)   = (TypeSpec . TypedefName . show) p
toC99Type (TNAProd n _)   = (TypeSpec . TypedefName) n

-- Print format
format :: TypeRepr -> String
format TInt    = "%d"
format TDouble = "%.14lf"
format TBool   = "%s"
format TChar   = "%c"

-- Includes
getIncludes :: TypeRepr -> [String]
getIncludes ty = findWithDefault [] ty includes
  where
    includes :: Map TypeRepr [String]
    includes = fromList [(TBool, ["<stdbool.h>"])]

-- C99able class

class C99able a where
  typeRepr :: TypeRepr


-- || Data types for C99 translation
 
{-
  The C99 translation type (C99) is implemented with a state monad, whose computations construct
  language-c99-simple AST values (expressions, declarations and statements) that implement
  the C99 translation for MCLola streams.
  
  The state, of type C99St, has 4 components:
   - the mapping from stream names to their type representation;
   - the identifier of the stream being computed (used for naming purposes);
   - an integer (used for fresh variable name generation);
   - the mapping from references names (let bindings) to their type representation and implementation name.
   
  The computed value, of type C99Exp, has 4 components:
   - the (language-c99-simple) expression that correspond to the value being translated;
   - the (language-c99-simple) declarations needed to define that expression (may be none);
   - the (language-c99-simple) statements needed to define that expression (may be none);
   - the type representation of the expression.
  
  Type C99Fun is used to represent translation of functions.
  C99Fun is a functional type, that given a list of arguments (of type C99Exp), 
  computes the C99 translation type corresponding to the evaluation
  of the function being translated, evaluated in those arguments.
-}

type C99St = (TypeMap, Ident, Int, BindMap)

type C99Exp = (Expr, [Decln], [Stmt], TypeRepr)

type C99 = State C99St C99Exp

type C99Fun = [C99Exp] -> C99


-- || Default values, used to satisfy Misra C restrictions

maxList = 30

getDefault :: TypeRepr -> C99
getDefault TInt          = return (LitInt 0, [], [], TInt)
getDefault TDouble       = return (LitDouble 0, [], [], TDouble)
getDefault TBool         = return (LitBool True, [], [], TBool)
getDefault TChar         = return (LitInt 0, [], [], TChar)
getDefault (TMaybe t)    = do
                            (e, d, s, _) <- getDefault t
                            (typeMap, str, n, bindMap) <- get
                            let
                               name     = str ++ "_" ++ show n
                               expr     = Ident name
                               decs     = d ++ [VarDecln Nothing (toC99Type (TMaybe t)) name Nothing]
                               assign   = [ Expr (AssignOp Assign (Dot expr "nothing") (LitInt 0))
                                          , Expr (AssignOp Assign (Dot expr "just") e)]
                               stmts    = s ++ assign
                            put (typeMap, str, n+1, bindMap)
                            return (expr, decs, stmts, TMaybe t)
getDefault (TEither t u) = do
                            (el, dl, sl, _) <- getDefault t
                            (er, dr, sr, _) <- getDefault u
                            (typeMap, str, n, bindMap) <- get
                            let
                               name     = str ++ "_" ++ show n
                               expr     = Ident name
                               decs     = dl ++ dr ++ [VarDecln Nothing (toC99Type (TEither t u)) name Nothing]
                               assign   = [ Expr (AssignOp Assign (Dot expr "left") el)
                                          , Expr (AssignOp Assign (Dot expr "right") er)
                                          , Expr (AssignOp Assign (Dot expr "opt") (LitInt 0))]
                               stmts    = sl ++ sr ++ assign
                            put (typeMap, str, n+1, bindMap)
                            return (expr, decs, stmts, TEither t u)
getDefault (TList t)   = do
                            (e, d, s, _) <- getDefault t
                            (typeMap, str, n, bindMap) <- get
                            let
                               name     = str ++ "_" ++ show n
                               idx      = "i_" ++ str ++ "_" ++ show n
                               expr     = Ident name
                               decs     = d ++ [VarDecln Nothing (toC99Type (TList t)) name Nothing, VarDecln Nothing (TypeSpec Int) idx (Just (InitExpr (LitInt 0)))]
                               assign   = [ Expr (AssignOp Assign (Dot expr "n") (LitInt 0))
                                          , Expr (AssignOp Assign (Dot expr "size") (LitInt (toInteger maxList)))
                                          , For (Ident idx)
                                                (BinaryOp LT (Ident idx) (LitInt (toInteger maxList)))
                                                (UnaryOp Inc (Ident idx))
                                                [Expr (AssignOp Assign (Index (Dot expr "data") (Ident idx)) e)]]
                               stmts    = s ++ assign
                            put (typeMap, str, n+1, bindMap)
                            return (expr, decs, stmts, TList t)
getDefault (TProd t u)   = do
                            (e1, d1, s1, _) <- (getDefault t)
                            (e2, d2, s2, _) <- (getDefault u)
                            (typeMap, str, n, bindMap) <- get
                            let
                               name     = str ++ "_" ++ show n
                               expr     = Ident name
                               decs     = d1 ++ d2 ++ [VarDecln Nothing (toC99Type (TProd t u)) name Nothing]
                               assign   = [ Expr (AssignOp Assign (Dot expr "c1") e1)
                                          , Expr (AssignOp Assign (Dot expr "c2") e2)]
                               stmts    = s1 ++ s2 ++ assign
                            put (typeMap, str, n+1, bindMap)
                            return (expr, decs, stmts, TProd t u)
getDefault (TNAProd nm fs) = do
                            results <- mapM getDefault (map snd fs)
                            (typeMap, str, n, bindMap) <- get
                            let
                               name     = str ++ "_" ++ show n
                               expr     = Ident name
                               decs     = (foldr (++) [] (map (\(_, d, _, _) -> d) results)) ++ [VarDecln Nothing (toC99Type (TNAProd nm fs)) name Nothing]
                               assign   = map (\((e, _, _, _), i) -> Expr (AssignOp Assign (Dot expr (fst (fs !! i))) e)) (zip results [0..])
                               stmts    = (foldr (++) [] (map (\(_, _, s, _) -> s) results)) ++ assign
                            put (typeMap, str, n+1, bindMap)
                            return (expr, decs, stmts, TNAProd nm fs)
