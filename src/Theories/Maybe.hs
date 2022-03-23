{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}

module Theories.Maybe where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.FreeVars
import Engine.Trans
import Internal.TypeRepr
import Internal.C99              (C99Fun, toC99Type, C99able(..))

import Data.Comp.Multi.Derive    (derive, makeHFunctor, makeEqHF, smartConstructors)
import Language.C99.Simple       as LC99
import Control.Monad.State.Lazy  (get, put)

{-

  Intermediate Representation for Maybe Data Theory.

* Types:

  Maybe t
  
* Functions:
  
  just      : Exp a -> Exp (Maybe a)
  nothing   : Exp a -> Exp (Maybe a)
  isNothing : Exp (Maybe a) -> Exp Bool
  fromJust  : Exp (Maybe a) -> Exp a -> Exp a
  
-}

-- | C99able instances

instance C99able a => C99able (Maybe a) where
  typeRepr = TMaybe (typeRepr @a)

-- | Data Theory

data MaybeTheory (e :: * -> *) (a :: *) where
  NothingM  :: e a -> MaybeTheory e (Maybe a) -- default for nothing
  JustM     :: e a -> MaybeTheory e (Maybe a)
  IsNothing :: e (Maybe a) -> MaybeTheory e Bool
  FromMaybe :: e (Maybe a) -> e a -> MaybeTheory e a

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [''MaybeTheory])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances

instance TempRefAlg MaybeTheory where
  tempRefAlg (NothingM x)    = TR $ unTR x
  tempRefAlg (JustM x)       = TR $ unTR x
  tempRefAlg (IsNothing x)   = TR $ unTR x
  tempRefAlg (FromMaybe x y) = TR $ unTR x ++ unTR y

-- | TypesAlg Instances

instance TypesAlg MaybeTheory where
  typesAlg (NothingM x) =
    Types (do (tsx, t) <- unTypes x
              return ([TMaybe t] ++ tsx, TMaybe t))
  typesAlg (JustM x) =
    Types (do (tsx, t) <- unTypes x
              return ([TMaybe t] ++ tsx, TMaybe t))
  typesAlg (IsNothing x) =
    Types (do (tsx, _) <- unTypes x
              return ([TBool] ++ tsx, TBool))
  typesAlg (FromMaybe x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, t) <- unTypes y
              return (tsx ++ tsy, t))

-- | FreeVarsAlg Instances

instance FreeVarsAlg MaybeTheory where
  freeVarsAlg (NothingM x)    = FreeVars $ unFreeVars x
  freeVarsAlg (JustM x)       = FreeVars $ unFreeVars x
  freeVarsAlg (IsNothing x)   = FreeVars $ unFreeVars x
  freeVarsAlg (FromMaybe x y) = FreeVars $ unFreeVars x ++ unFreeVars y

-- | C99DefAlg Instances

instance C99DefAlg MaybeTheory where
  c99DefAlg (NothingM x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Nothing) [arg1])
  c99DefAlg (JustM x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Just) [arg1])
  c99DefAlg (IsNothing x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99IsNothing) [arg1])
  c99DefAlg (FromMaybe x y) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99FromMaybe) [arg1, arg2])

c99Nothing :: C99Fun
c99Nothing args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TMaybe (pr4 (args !! 0))
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "just") (pr1 (args !! 0)))
                , Expr (AssignOp Assign (Dot expr "nothing") (LitInt 1))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99Just :: C99Fun
c99Just args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TMaybe (pr4 (args !! 0))
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "just") (pr1 (args !! 0)))
                , Expr (AssignOp Assign (Dot expr "nothing") (LitInt 0))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99IsNothing :: C99Fun
c99IsNothing args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TBool
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [Expr (AssignOp Assign expr (BinaryOp LC99.Eq (Dot (pr1 (args !! 0)) "nothing") (LitInt 1)))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99FromMaybe :: C99Fun
c99FromMaybe args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = pr4 (args !! 1)
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [IfElse (BinaryOp LC99.Eq (Dot (pr1 (args !! 0)) "nothing") (LitInt 0))
                   [Expr (AssignOp Assign expr (Dot (pr1 (args !! 0)) "just"))]
                   [Expr (AssignOp Assign expr (pr1 (args !! 1)))]]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
