{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts #-}

module Theories.LetBind where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.FreeVars
import Engine.Trans
import Internal.Maps
import Internal.C99              (C99Fun, toC99Type)

import Data.Comp.Multi.Derive    (derive, makeHFunctor, makeEqHF, smartConstructors)
import Language.C99.Simple       as LC99 hiding (Ref)
import Data.Maybe                (fromMaybe)
import Data.Map                  (insert, lookup, keys)
import Data.List                 ((\\))
import Control.Monad.State.Lazy  (get, put)
import Prelude                   hiding (lookup)

{-

  Intermediate Representation for Let Bindings Data Theory.
  (Internal Data Theory)

* Types:

  -
  
* Functions:
  
  let : Identifier -> Exp a -> Exp b -> Exp b
  ref : Identifier -> Exp a

-}

-- | Signatures for abstractions to implement let binding

data LetBind e a where
  Let :: String -> e a -> e b -> LetBind e b
  Ref :: String -> LetBind e a

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [''LetBind])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances

instance TempRefAlg LetBind where
  tempRefAlg (Let s x y) = TR $ unTR x ++ unTR y
  tempRefAlg (Ref s)     = TR []

-- | TypesAlg Instances

instance TypesAlg LetBind where
  typesAlg (Let s x y) =
    Types (do (mpS, mpL) <- get
              (tsx, tx) <- unTypes x
              put (mpS, insert s tx mpL)
              (tsy, ty) <- unTypes y
              put (mpS, mpL)
              return (tsx ++ tsy, ty))
  typesAlg (Ref s) =
    Types (do (_, mp) <- get
              let t = fromMaybe (error ("err: " ++ s ++ " - " ++ (show (keys mp)))) (lookup s mp)
              return ([t], t))
            
-- | FreeVarsAlg Instances

instance FreeVarsAlg LetBind where
  freeVarsAlg (Let s x y) = FreeVars $ (unFreeVars x) ++ ((unFreeVars y) \\ [s])
  freeVarsAlg (Ref s)     = FreeVars [s]

-- | C99DefAlg Instances

instance C99DefAlg LetBind where
  c99DefAlg (Let s x y) =
    FC99 (do arg1 <- unC99 x
             (typeMap, str, n, bindMap) <- get
             let
                 ty  = pr4 arg1
                 newBindMap = insertS s (ty, s ++ "_" ++ show n) bindMap
             put (typeMap, str, n+1, newBindMap)
             arg2 <- unC99 y
             c99Let s [arg1, arg2])
  c99DefAlg (Theories.LetBind.Ref s) =
    FC99 (c99Ref s [])

c99Let :: String -> C99Fun
c99Let s args =
  do (typeMap, str, n, bindMap) <- get
     let 
         expr = pr1 (args !! 1)
         ty   = pr4 (args !! 1)
         (tyl, name) = fromMaybe (error "err") (lookupS s bindMap)
         decs = (pr2 (args !! 0)) ++ [VarDecln Nothing (toC99Type tyl) name Nothing] ++ (pr2 (args !! 1))
         stms = (pr3 (args !! 0)) ++ [Expr (AssignOp Assign (Ident name) (pr1 (args !! 0)))] ++  (pr3 (args !! 1))
         newBindMap = deleteS s bindMap
     put (typeMap, str, n, newBindMap)
     return (expr, decs, stms, ty)
  
c99Ref :: String -> C99Fun
c99Ref s args =
  do (typeMap, str, n, bindMap) <- get
     let 
         (ty, name) = fromMaybe (error ("err: " ++ s)) (lookupS s bindMap)
     return (Ident name, [], [], ty)
