module Examples.Numeric.Abs.Spec (spec) where

import MCLola
import Theories.Numeric

{-  Specification
    
 -- Input
 
 input int a
 input double b
 
 -- Output
 
 output int x    = abs a
 output double y = abs b[2|0]
 
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
    expr = iIAbs (iNow a)

y :: Stream Double
y = Out ("y", expr)
  where
    expr = iDAbs (iOffset b 2 (iValDouble 0))

-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm b
       , toDynStrm x
       , toDynStrm y]
