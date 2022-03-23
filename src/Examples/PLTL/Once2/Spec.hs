module Examples.PLTL.Once2.Spec (spec) where

import MCLola
import Lib.PLTL

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
once_p = once p

-- | Specification

spec :: Specification
spec = [ toDynStrm p
       , toDynStrm once_p ]
