{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts #-}

module Theories.Bool where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.FreeVars
import Engine.Trans
import Internal.TypeRepr
import Internal.C99              (C99, C99Fun, toC99Type, getDefault, C99able(..))
import Theories.Simpl            (comm)

import Data.Comp.Multi.Derive    (derive, makeHFunctor, makeEqHF, smartConstructors)
import Language.C99.Simple       as LC99
import Prelude                   hiding (Eq)
import Control.Monad.State.Lazy  (get, put)

{-

  Intermediate Representation for Boolean Data Theory.

* Types:

  Bool
  
* Functions:
  
  lit  : Bool -> Exp Bool
  not  : Exp Bool -> Exp Bool
  or   : Exp Bool -> Exp Bool -> Exp Bool
  and  : Exp Bool -> Exp Bool -> Exp Bool
  impl : Exp Bool -> Exp Bool -> Exp Bool
  if   : Exp Bool -> Exp a -> Exp a -> Exp a
  eq   : Exp a -> Exp a -> Exp Bool
  
-}

-- | C99able instance

instance C99able Bool where
  typeRepr = TBool
  
-- | Data Theory

data BoolTheory (e :: * -> *) (a :: *) where
  ValBool :: Bool -> BoolTheory e Bool
  Not     :: e Bool -> BoolTheory e Bool
  Or      :: e Bool -> e Bool -> BoolTheory e Bool
  And     :: e Bool -> e Bool -> BoolTheory e Bool
  Impl    :: e Bool -> e Bool -> BoolTheory e Bool
  Eq      :: e a -> e a -> BoolTheory e Bool
  Ite     :: e Bool -> e a -> e a -> BoolTheory e a

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [''BoolTheory])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances
  
instance TempRefAlg BoolTheory where
  tempRefAlg (ValBool _)             = TR []
  tempRefAlg (Ite b x y)             = TR $ unTR b ++ unTR x ++ unTR y
  tempRefAlg (Theories.Bool.Eq x y)  = TR $ unTR x ++ unTR y
  tempRefAlg (Theories.Bool.Not x)   = TR $ unTR x
  tempRefAlg (Theories.Bool.Or x y)  = TR $ unTR x ++ unTR y
  tempRefAlg (Theories.Bool.And x y) = TR $ unTR x ++ unTR y
  tempRefAlg (Impl x y)              = TR $ unTR x ++ unTR y
  
-- | TypesAlg Instances
  
instance TypesAlg BoolTheory where
  typesAlg (ValBool _) = Types $ return ([TBool], TBool)
  typesAlg (Ite b x y) =
    Types (do (tsb, _) <- unTypes b
              (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsb ++ tsx ++ tsy, t))
  typesAlg (Theories.Bool.Eq x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (Theories.Bool.Not x) =
    Types (do (tsx, _) <- unTypes x
              return (tsx, TBool))
  typesAlg (Theories.Bool.Or x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, TBool))
  typesAlg (Theories.Bool.And x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, TBool))
  typesAlg (Impl x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, TBool))

-- | FreeVarsAlg Instances
  
instance FreeVarsAlg BoolTheory where
  freeVarsAlg (ValBool _)             = FreeVars []
  freeVarsAlg (Ite b x y)             = FreeVars $ unFreeVars b ++ unFreeVars x ++ unFreeVars y
  freeVarsAlg (Theories.Bool.Eq x y)  = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (Theories.Bool.Not x)   = FreeVars $ unFreeVars x
  freeVarsAlg (Theories.Bool.Or x y)  = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (Theories.Bool.And x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (Impl x y)              = FreeVars $ unFreeVars x ++ unFreeVars y
  
-- | C99DefAlg Instances

instance C99DefAlg BoolTheory where
  c99DefAlg (ValBool b) = FC99 $ (toMaybeVal . c99ValBool) b
  c99DefAlg (Ite b x y) =
    FC99 (do arg1 <- unC99 b
             arg2 <- unC99 x
             arg3 <- unC99 y
             (toMaybeFun c99Ite) [arg1, arg2, arg3])
  c99DefAlg (Theories.Bool.Eq x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Eq) [arg1, arg2])
  c99DefAlg (Theories.Bool.Not x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Not) [arg1])
  c99DefAlg (Theories.Bool.Or x y) = c99DefAlg (orOp x y)
    where
      orOp e1 e2 = Ite e1 e1 e2
  c99DefAlg (Theories.Bool.And x y) = c99DefAlg (andOp x y)
    where
      andOp e1 e2 = Ite e1 e2 e1
  c99DefAlg (Theories.Bool.Impl x y) = c99DefAlg (implOp x y)
    where
      implOp e1 e2 = Ite e1 e2 (c99DefAlg (ValBool True))
  
c99ValBool :: Bool -> C99
c99ValBool b = return (LitBool b, [], [], TBool)

c99Ite :: C99Fun
c99Ite args = return (Cond (pr1 (args !! 0)) (pr1 (args !! 1)) (pr1 (args !! 2)), [], [], pr4 (args !! 1))

c99Eq :: C99Fun
c99Eq args = return (Funcall (Ident ("equals" ++ show (pr4 (args !! 0)))) [pr1 (args !! 0), pr1 (args !! 1)], [], [], TBool)

c99Not :: C99Fun
c99Not args = return (UnaryOp LC99.Not (pr1 (args !! 0)), [], [], TBool)

-- | C99SimAlg Instances

instance C99SimAlg BoolTheory where
  c99SimAlg (Ite b x y) =
    FC99 (do arg1 <- unC99 b
             arg2 <- unC99 x
             arg3 <- unC99 y
             c99IteS [arg1, arg2, arg3])
  c99SimAlg (Theories.Bool.Or x y) = c99SimAlg (comm orOp x y)
    where
      orOp e1 e2 = Ite e1 e1 e2
  c99SimAlg (Theories.Bool.And x y) = c99SimAlg (comm andOp x y)
    where
      andOp e1 e2 = Ite e1 e2 e1
  c99SimAlg (Theories.Bool.Impl x y) = c99SimAlg (implOp x y)
    where
      implOp e1 e2 = Ite e1 e2 (c99SimAlg (ValBool True))
  c99SimAlg x = c99DefAlg x
             
c99IteS :: C99Fun
c99IteS args =
  do let 
         ty   = pr4 (args!!1)
         typ  = fromTMaybe ty
     (dexpr, ddecs, dstms, _)   <- getDefault typ
     (typeMap, str, n, bindMap) <- get
     let
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = foldr (++) [] (map pr2 args) ++ ddecs ++ [VarDecln Nothing (toC99Type ty) name Nothing]
         
         just n = BinaryOp LC99.Eq (Dot (pr1 (args !! n)) "nothing") (LitInt 0)
         cond b = BinaryOp LC99.LAnd (just 0) (BinaryOp LC99.Eq (Dot (pr1 (args !! 0)) "just") (LitBool b))
         eqJst  = Funcall (Ident ("equals" ++ show typ)) [Dot (pr1 (args !! 1)) "just", Dot (pr1 (args !! 2)) "just"]
         eqBr   = BinaryOp LC99.LAnd (BinaryOp LC99.LAnd (just 1) (just 2)) eqJst
         unknown = [ Expr (AssignOp Assign (Dot expr "nothing") (LitInt 1))
                   , Expr (AssignOp Assign (Dot expr "just") dexpr)]
         known n = [ Expr (AssignOp Assign (Dot expr "nothing") (Dot (pr1 (args !! n)) "nothing"))
                   , Expr (AssignOp Assign (Dot expr "just") (Dot (pr1 (args !! n)) "just"))]
                   
         stmt = [IfElse (cond True)
                   (known 1)
                   [IfElse (cond False)
                      (known 2)
                      [IfElse eqBr
                         (known 1)
                         unknown]]]
         stms = foldr (++) [] (map pr3 args) ++ dstms ++ stmt
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
