{-# Language GADTs, TemplateHaskell #-}

module StaticAnalysis.TempRef where

import Data.Comp.Multi         (Term, Alg, cata)
import Data.Comp.Multi.Derive  (derive, HFunctor, liftSum)

{-

  Temporal references computation

-}

-- Vertex of dependency graph
type Vert = String

-- || Temporal references computation

{-
  Datatype for temporal references computation.
  For every expression e, we want to compute a list of the streams that
  take part in e's definition, together with the corresponding offset.
-}

data TempRef e where
  TR :: [(Vert, Int)] -> TempRef e
  
unTR (TR x) = x

class TempRefAlg e where
  tempRefAlg :: Alg e TempRef

computeTempRef :: (HFunctor e, TempRefAlg e) => Term e a -> [(Vert, Int)]
computeTempRef t = unTR (cata tempRefAlg t)

-- Derive boilerplate code using Template Haskell
$(derive [liftSum] [''TempRefAlg])
