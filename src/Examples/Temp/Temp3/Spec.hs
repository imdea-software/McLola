module Examples.Temp.Temp3.Spec (spec) where

import MCLola
import Theories.Numeric

{-  Specification
    
 -- Input
 
 input int a
 input int b
 
 -- Output
 
 output int x = y[-2|0] + a[1|0]
 output int y = b[3|0]
  
-}

-- | Input

a :: Stream Int
a = In "a"

b :: Stream Int
b = In "b"

-- | Output

x :: Stream Int
x = Out ("x", expr)
  where
    expr = iIAdd (iOffset y (-2) zero) (iOffset a 1 zero)
    zero = iValInt 0

y :: Stream Int
y = Out ("y", expr)
  where
    expr = iOffset b 3 zero
    zero = iValInt 0

-- | Specification

spec :: Specification
spec = [ toDynStrm x
       , toDynStrm b
       , toDynStrm a
       , toDynStrm y]
