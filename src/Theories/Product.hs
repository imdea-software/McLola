{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, TypeApplications, TypeOperators, FlexibleInstances #-}

module Theories.Product where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.Rewrite
import Engine.FreeVars
import Engine.Trans
import Internal.TypeRepr
import Internal.C99              (C99Fun, toC99Type, C99able(..))

import Data.Comp.Multi           ((:<:), project)
import Data.Comp.Multi.Derive    (derive, makeHFunctor, makeEqHF, smartConstructors)
import Language.C99.Simple       as LC99
import Control.Monad.State.Lazy  (get, put)

{-

  Intermediate Representation for Product Data Theory.
  
* Types:

  (t,u)
  
* Functions:

  fst      : Exp (a, b) -> Exp a
  snd      : Exp (a, b) -> Exp b
  makePair : Exp a -> Exp b -> Exp (a, b)
  
-}

-- | C99able instances

instance (C99able a, C99able b) => C99able (a, b) where
  typeRepr = TProd (typeRepr @a) (typeRepr @b)
  
-- | Data Theory
  
data ProdTheory e a where
  Fst  :: e (a, b) -> ProdTheory e a
  Snd  :: e (a, b) -> ProdTheory e b
  Pair :: e a -> e b -> ProdTheory e (a, b)

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [''ProdTheory])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances
  
instance TempRefAlg ProdTheory where
  tempRefAlg (Fst x)    = TR $ unTR x
  tempRefAlg (Snd x)    = TR $ unTR x
  tempRefAlg (Pair x y) = TR $ unTR x ++ unTR y
  
-- | TypesAlg Instances

instance TypesAlg ProdTheory where
  typesAlg (Fst x) =
    Types (do (tsx, tx) <- unTypes x
              case tx of
                TProd t _ -> return ([t]++tsx, t)
                _         -> error "type error")
  typesAlg (Snd x) =
    Types (do (tsx, tx) <- unTypes x
              case tx of
                TProd _ t -> return ([t]++tsx, t)
                _         -> error "type error")
  typesAlg (Pair x y) =
    Types (do (tsx, tx) <- unTypes x
              (tsy, ty) <- unTypes y
              return ([TProd tx ty] ++ tsx ++ tsy, TProd tx ty))

-- | FreeVarsAlg Instances
  
instance FreeVarsAlg ProdTheory where
  freeVarsAlg (Fst x)    = FreeVars $ unFreeVars x
  freeVarsAlg (Snd x)    = FreeVars $ unFreeVars x
  freeVarsAlg (Pair x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  
-- | Rewrite Instances

instance (ProdTheory :<: v) => Rewrite ProdTheory v where
  rewriteAlg (Fst x) = case project x of
    Just (Pair a b) -> a
    Nothing         -> iFst x
  rewriteAlg (Snd x) = case project x of
    Just (Pair a b) -> b
    Nothing         -> iSnd x
  rewriteAlg (Pair x y) = iPair x y
    
-- | C99DefAlg Instances

instance C99DefAlg ProdTheory where
  c99DefAlg (Fst x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Fst) [arg1])
  c99DefAlg (Snd x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Snd) [arg1])
  c99DefAlg (Pair x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Pair) [arg1, arg2])
  
c99Fst :: C99Fun
c99Fst args = case (pr4 (args !! 0)) of
                TProd ty _ -> return (Dot (pr1 (args !! 0)) "c1", [], [], ty)
                _          -> error "err"
  
c99Snd :: C99Fun
c99Snd args = case (pr4 (args !! 0)) of
                TProd _ ty -> return (Dot (pr1 (args !! 0)) "c2", [], [], ty)
                _          -> error "err"

c99Pair :: C99Fun
c99Pair args = do
                (typeMap, str, n, bindMap) <- get
                let
                    name  = str ++ "_" ++ show n
                    expr  = Ident name
                    ty    = TProd (pr4 (args !! 0)) (pr4 (args !! 1))
                    decs  = [VarDecln Nothing (toC99Type ty) name Nothing]
                    stmts = map (\(e, n) -> Expr (AssignOp Assign (Dot (Ident name) ("c"++(show n))) e)) (zip (map pr1 args) [1..])
                put (typeMap, str, n+1, bindMap)
                return (expr, decs, stmts, ty)
