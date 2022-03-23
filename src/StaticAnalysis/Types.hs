{-# Language GADTs, TemplateHaskell #-}

module StaticAnalysis.Types where

import Internal.TypeRepr
import Internal.Maps

import Data.Comp.Multi         (Term, Alg, cata)
import Data.Comp.Multi.Derive  (derive, HFunctor, liftSum)
import Control.Monad.State     (State)

{-

  Types computation

-}

-- || Intermediate types computation

{-
  Datatype for intermediate types computation.
  The first component of state represents the mapping from stream names to their types, and
  the second component of state represents the mapping from let variables to their types.
  The State monad is used (instead of Reader monad) to handle let bindings.
-}

data Types e where
  Types :: State (TypeMap, TypeMap) ([TypeRepr], TypeRepr) -> Types e

unTypes (Types x) = x

class TypesAlg e where
  typesAlg :: Alg e Types

exprTypes :: (HFunctor e, TypesAlg e) => Term e a -> State (TypeMap, TypeMap) ([TypeRepr], TypeRepr)
exprTypes t = unTypes (cata typesAlg t)

-- Derive boilerplate code using Template Haskell
$(derive [liftSum] [''TypesAlg])
