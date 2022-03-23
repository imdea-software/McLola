module Examples.PLTL.Once.Spec (spec) where

import MCLola
import Theories.Bool

{-  Specification
    
 -- Input
 
 input bool p
 
 -- Output

 output bool once_p = once_p[-1|false] \/ p

-}

-- | Input

p :: Stream Bool
p = In "p"

-- | Output

once_p :: Stream Bool
once_p = Out ("once_p", expr)
  where
    expr = iOr (iOffset once_p (-1) (iValBool False)) (iNow p)

-- | Specification

spec :: Specification
spec = [ toDynStrm p
       , toDynStrm once_p ]
