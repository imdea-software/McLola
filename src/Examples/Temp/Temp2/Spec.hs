module Examples.Temp.Temp2.Spec (spec) where

import MCLola
import Theories.Numeric

{-  Specification
    
 -- Input
 
 input int a
 
 -- Output
 
 output int x = a[3|0] + 5
 output int y = y[-1|0] + (a + (a[-2|1] - x[1|1])) * 2
  
-}

-- | Input

a :: Stream Int
a = In "a"

-- | Output

x :: Stream Int
x = Out ("x", expr)
  where
    expr = iIAdd (iOffset a 3 dflt) (iValInt 5)
    dflt = iValInt 0

y :: Stream Int
y = Out ("y", expr)
  where
    expr = exp1 `iIAdd` (exp2 `iIMul` exp3)
    exp1 = iOffset y (-1) dflt0
    exp2 = (iNow a) `iIAdd` (iOffset a (-2) dflt1) `iISub` (iOffset x 1 dflt1)
    exp3 = iValInt 2
    dflt0 = iValInt 0
    dflt1 = iValInt 1

-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm x
       , toDynStrm y]
