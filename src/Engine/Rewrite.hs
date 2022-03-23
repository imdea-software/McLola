{-# LANGUAGE GADTs, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Engine.Rewrite where

import Engine.FreeVars         (FreeVarsAlg)

import Data.Comp.Multi         (Term, Alg, cata, inject, (:<:), (:->))
import Data.Comp.Multi.Derive  (derive, HFunctor, liftSum)

{-

  Rewrite of IR expressions for anticipation and optimization purposes
  
-}

-- || Rewrite

class Rewrite e v where
  rewriteAlg :: (FreeVarsAlg v, HFunctor v) => Alg e (Term v)

rewrite :: (HFunctor e, Rewrite e e, FreeVarsAlg e) => Term e :-> Term e
rewrite = cata rewriteAlg

-- Default instance, intended for constructors without rewriting rules
instance {-# OVERLAPPABLE #-} (e :<: v) => Rewrite e v where
  rewriteAlg = inject

-- Derive boilerplate code using Template Haskell
$(derive [liftSum] [''Rewrite])
