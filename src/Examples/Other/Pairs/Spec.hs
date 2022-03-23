module Examples.Other.Pairs.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric
import Theories.Product

{-  Specification
    
 -- Input
 
 input int a
 
 -- Output
 
 output (bool, int) w = (x < y, x)
 output int x = fst (a, a[3|0])
 output int y = snd (2.0, a[3|0])
 output bool z = snd (a, a>=4 \/ a[3|0]>=4)
 
-}

-- | Input

a :: Stream Int
a = In "a"

-- | Output

w :: Stream (Bool, Int)
w = Out ("w", expr)
  where
    expr = iPair (iILt (iNow x) (iNow y)) (iNow x)

x :: Stream Int
x = Out ("x", expr)
  where
    expr = iFst (iPair (iNow a) (iOffset a 3 dflt))

y :: Stream Int
y = Out ("y", expr)
  where
    expr = iSnd (iPair (iValDouble 2) (iOffset a 3 dflt))
    
z :: Stream Bool
z = Out ("z", expr)
  where
    aux  = iOr (iIGeq (iNow a) (iValInt 4)) (iIGeq (iOffset a 3 dflt) (iValInt 4))
    expr = iSnd (iPair (iNow a) aux)

dflt = iValInt 0

-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm w
       , toDynStrm x
       , toDynStrm y
       , toDynStrm z]
