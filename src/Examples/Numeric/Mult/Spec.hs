module Examples.Numeric.Mult.Spec (spec) where

import MCLola
import Theories.Numeric

{-  Specification
    
 -- Input
 
 input int a
 input double b
 
 -- Output
 
 output int x    = a * a[2|0]
 output double y = b * b[2|0]
 
-}

-- | Input

a :: Stream Int
a = In "a"

b :: Stream Double
b = In "b"

-- | Output

x :: Stream Int
x = Out ("x", expr)
  where
    expr = iIMul (iNow a) (iOffset a 2 dflt)
    dflt = iValInt 0

y :: Stream Double
y = Out ("y", expr)
  where
    expr = iDMul (iNow b) (iOffset b 2 dflt)
    dflt = iValDouble 0

-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm b
       , toDynStrm x
       , toDynStrm y]
