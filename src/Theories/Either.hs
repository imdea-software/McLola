{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}

module Theories.Either where

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

  Intermediate Representation for Coproduct Data Theory.

* Types:

  Either t u
  
* Functions:
  
  eitherL   : Exp a -> Exp b -> Exp (Either a b)
  eitherR   : Exp a -> Exp b -> Exp (Either a b)  
  isLeft    : Exp (Either a b) -> Exp Bool
  isRight   : Exp (Either a b) -> Exp Bool
  fromLeft  : Exp (Either a b) -> Exp a
  fromRight : Exp (Either a b) -> Exp b
  
-}

-- | C99able instances

instance (C99able a, C99able b) => C99able (Either a b) where
  typeRepr = TEither (typeRepr @a) (typeRepr @b)
  
-- | Data Theory

data EitherTheory (e :: * -> *) (a :: *) where
  EitherL   :: e a -> e b -> EitherTheory e (Either a b) -- default for right
  EitherR   :: e a -> e b -> EitherTheory e (Either a b) -- default for left
  IsLeft    :: e (Either a b) -> EitherTheory e Bool
  IsRight   :: e (Either a b) -> EitherTheory e Bool
  FromLeft  :: e (Either a b) -> EitherTheory e a
  FromRight :: e (Either a b) -> EitherTheory e b

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [''EitherTheory])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances

instance TempRefAlg EitherTheory where
  tempRefAlg (EitherL x y) = TR $ unTR x ++ unTR y
  tempRefAlg (EitherR x y) = TR $ unTR x ++ unTR y
  tempRefAlg (IsLeft x)    = TR $ unTR x
  tempRefAlg (IsRight x)   = TR $ unTR x
  tempRefAlg (FromLeft x)  = TR $ unTR x
  tempRefAlg (FromRight x) = TR $ unTR x

-- | TypesAlg Instances

instance TypesAlg EitherTheory where
  typesAlg (EitherL x y) =
    Types (do (tsx, tl) <- unTypes x
              (tsy, tr) <- unTypes y
              return ([TEither tl tr] ++ tsx ++ tsy, TEither tl tr))
  typesAlg (EitherR x y) =
    Types (do (tsx, tl) <- unTypes x
              (tsy, tr) <- unTypes y
              return ([TEither tl tr] ++ tsx ++ tsy, TEither tl tr))
  typesAlg (IsLeft x) =
    Types (do (tsx, _) <- unTypes x
              return ([TBool] ++ tsx, TBool))
  typesAlg (IsRight x) =
    Types (do (tsx, _) <- unTypes x
              return ([TBool] ++ tsx, TBool))
  typesAlg (FromLeft x) =
    Types (do (tsx, ty) <- unTypes x
              let tl = case ty of 
                         TEither u _ -> u
                         _           -> error "type error"
              return (tsx, tl))
  typesAlg (FromRight x) =
    Types (do (tsx, ty) <- unTypes x
              let tr = case ty of 
                         TEither _ u -> u
                         _           -> error "type error"
              return (tsx, tr))

-- | FreeVarsAlg Instances

instance FreeVarsAlg EitherTheory where
  freeVarsAlg (EitherL x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (EitherR x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (IsLeft x)    = FreeVars $ unFreeVars x
  freeVarsAlg (IsRight x)   = FreeVars $ unFreeVars x
  freeVarsAlg (FromLeft x)  = FreeVars $ unFreeVars x
  freeVarsAlg (FromRight x) = FreeVars $ unFreeVars x

-- | C99DefAlg Instances

instance C99DefAlg EitherTheory where
  c99DefAlg (EitherL x y) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99EitherL) [arg1, arg2])
  c99DefAlg (EitherR x y) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99EitherR) [arg1, arg2])
  c99DefAlg (IsLeft x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99IsLeft) [arg1])
  c99DefAlg (IsRight x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99IsRight) [arg1])
  c99DefAlg (FromLeft x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99FromLeft) [arg1])
  c99DefAlg (FromRight x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99FromRight) [arg1])
  
c99EitherL :: C99Fun
c99EitherL args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TEither (pr4 (args !! 0)) (pr4 (args !! 1))
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "left") (pr1 (args !! 0)))
                , Expr (AssignOp Assign (Dot expr "right") (pr1 (args !! 1)))
                , Expr (AssignOp Assign (Dot expr "opt") (LitInt 0))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99EitherR :: C99Fun
c99EitherR args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TEither (pr4 (args !! 0)) (pr4 (args !! 1))
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "left") (pr1 (args !! 0)))
                , Expr (AssignOp Assign (Dot expr "right") (pr1 (args !! 1)))
                , Expr (AssignOp Assign (Dot expr "opt") (LitInt 1))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)

  
c99IsLeft :: C99Fun
c99IsLeft args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TBool
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [Expr (AssignOp Assign expr (BinaryOp LC99.Eq (Dot (pr1 (args !! 0)) "opt") (LitInt 0)))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99IsRight :: C99Fun
c99IsRight args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TBool
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [Expr (AssignOp Assign expr (BinaryOp LC99.Eq (Dot (pr1 (args !! 0)) "opt") (LitInt 1)))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99FromLeft :: C99Fun
c99FromLeft args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = case (pr4 (args !! 0)) of
                  TEither t _ -> t
                  _           -> error "err"
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [Expr (AssignOp Assign expr (Dot (pr1 (args !! 0)) "left"))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99FromRight :: C99Fun
c99FromRight args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = case (pr4 (args !! 0)) of
                  TEither _ t -> t
                  _           -> error "err"
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [Expr (AssignOp Assign expr (Dot (pr1 (args !! 0)) "right"))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
