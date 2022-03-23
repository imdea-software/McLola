{-# LANGUAGE GADTs, TemplateHaskell, FlexibleInstances, UndecidableInstances #-}

module Engine.Trans where

import Engine.Rewrite            (rewrite, Rewrite)
import Engine.FreeVars           (FreeVarsAlg)
import Internal.TypeRepr         (TypeRepr(..))
import Internal.C99              (C99, C99Fun, toC99Type, getDefault)

import Data.Comp.Multi           (Term, Alg, cata)
import Data.Comp.Multi.Derive    (derive, HFunctor, EqHF, liftSum)
import Language.C99.Simple       as LC99
import Control.Monad.State.Lazy  (get, put)

{-

  Translation from IR expressions to C99 expressions

-}

-- || Translation

data FC99 e where
  FC99 :: C99 -> FC99 e

unC99 (FC99 x) = x

-- Default translation

class C99DefAlg e where
  c99DefAlg :: Alg e FC99

transDef :: (HFunctor e, C99DefAlg e, Rewrite e e, EqHF e, FreeVarsAlg e) => Term e a -> C99
transDef t = unC99 (cata c99DefAlg (fix rewrite t))

-- Simplifier translation

class C99SimAlg e where
  c99SimAlg :: Alg e FC99

transSim :: (HFunctor e, C99SimAlg e, Rewrite e e, EqHF e, FreeVarsAlg e) => Term e a -> C99
transSim t = unC99 (cata c99SimAlg (fix rewrite t))

-- Fixpoint for rewriting
fix :: Prelude.Eq a => (a -> a) -> a -> a
fix f x = if (f x == x) then x else fix f (f x)

-- Derive boilerplate code using Template Haskell
$(derive [liftSum] [''C99DefAlg, ''C99SimAlg])

     
-- || Instances for Simplifier translation

-- Default instance, intended for constructors with no simplifier version
instance {-# OVERLAPPABLE #-} C99DefAlg e => C99SimAlg e where
  c99SimAlg x = c99DefAlg x


-- || Auxiliar translation functions

toMaybeVal :: C99 -> C99
toMaybeVal c = 
  do (e, edecs, estms, ty) <- c
     (typeMap, str, n, bindMap) <- get
     let
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = edecs ++ [VarDecln Nothing (toC99Type (TMaybe ty)) name Nothing]
         assg =   [Expr (AssignOp Assign (Dot expr "nothing") (LitInt 0))]
               ++ [Expr (AssignOp Assign (Dot expr "just") e)]
         stms = estms ++ assg
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, TMaybe ty)
   
toMaybeFun :: C99Fun -> C99Fun
toMaybeFun f args = 
  do let
         someArgs = map (\(e, ds, ss, t) -> (Dot e "just", ds, ss, fromTMaybe t)) args
     (e, edecs, estms, ty) <- f someArgs
     (ed, dd, sd, _)       <- getDefault ty
     (typeMap, str, n, bindMap) <- get
     let
         name     = str ++ "_" ++ show n
         expr     = Ident name
         decs     = (foldr (++) [] (map pr2 args)) ++ edecs ++ dd ++ [VarDecln Nothing (toC99Type (TMaybe ty)) name Nothing]
         cond     = foldr (\arg exp -> BinaryOp LC99.LAnd exp (BinaryOp LC99.Eq (Dot (pr1 arg) "nothing") (LitInt 0))) (LitBool True) args
         someR    = estms ++
                    [ Expr (AssignOp Assign (Dot expr "nothing") (LitInt 0))
                    , Expr (AssignOp Assign (Dot expr "just") e)]
         noneR    = sd ++
                    [ Expr (AssignOp Assign (Dot expr "nothing") (LitInt 1))
                    , Expr (AssignOp Assign (Dot expr "just") ed)]
         condStmt = IfElse cond someR noneR
         stmts    = (foldr (++) [] (map pr3 args)) ++ [condStmt]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stmts, TMaybe ty)

    
fromTMaybe :: TypeRepr -> TypeRepr
fromTMaybe (TMaybe ty) = ty
fromTMaybe _           = error "Not maybe type"

pr1 :: (a,b,c,d) -> a
pr1(x, _, _, _) = x

pr2 :: (a,b,c,d) -> b
pr2(_, x, _, _) = x

pr3 :: (a,b,c,d) -> c
pr3(_, _, x, _) = x

pr4 :: (a,b,c,d) -> d
pr4(_, _, _, x) = x
     
