module Examples.Temp.FutAdd.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric

{-  Specification
    
 -- Input
 
 input int a
 
 -- Output
 
 output int x = a + a[1|0]
 output int y = if (x[1|0] >= x) then a[1|0] else a[-1|0]
 output int z = z[-1|0] + y[1|0]
  
-}

-- | Input

a :: Stream Int
a = In "a"

-- | Output

x :: Stream Int
x = Out ("x", expr)
  where
    expr = iIAdd (iNow a) (iOffset a 1 dflt)

y :: Stream Int
y = Out ("y", expr)
  where
    expr = iIte (iIGeq (iOffset x 1 dflt) (iNow x)) (iOffset a 1 dflt) (iOffset a (-1) dflt)

z :: Stream Int
z = Out ("z", expr)
  where
    expr = iIAdd (iOffset z (-1) dflt) (iOffset y 1 dflt)

dflt = iValInt 0

-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm x
       , toDynStrm y
       , toDynStrm z]
