module Examples.Temperature.Spec (spec) where

import MCLola
import Theories.Numeric
import Theories.Bool

{-  Specification
    
 -- Input
 
 input double temp
 
 -- Output
 
 output int high = high[-1|0] + if temp > 20 then 1 else 0
 
-}

-- | Input

temp :: Stream Double
temp = In "temp"

-- | Output

high :: Stream Int
high = Out ("high", exp)
  where
    exp = iIAdd e0 e1
    e0 = iOffset high (-1) (iValInt 0)
    e1 = iIte (iDGt (iNow temp) (iValDouble 20)) (iValInt 1) (iValInt 0)

-- | Specification

spec :: Specification
spec = [toDynStrm temp, toDynStrm high]
