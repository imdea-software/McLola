module Examples.Temp.FutOr.Spec (spec) where

import MCLola
import Theories.Bool

{-  Specification
    
 -- Input
 
 input bool a
 
 -- Output
 
 output bool x = a \/ a[1|false]
 output bool y = if (x[1|false] => x) then a[-1|false] else a[1|false]
 output bool z = z[-1|false] \/ y[1|false]
  
-}

-- | Input

a :: Stream Bool
a = In "a"

-- | Output

x :: Stream Bool
x = Out ("x", expr)
  where
    expr = iOr (iNow a) (iOffset a 1 dflt)

y :: Stream Bool
y = Out ("y", expr)
  where
    expr = iIte (iImpl (iOffset x 1 dflt) (iNow x)) (iOffset a (-1) dflt) (iOffset a 1 dflt)

z :: Stream Bool
z = Out ("z", expr)
  where
    expr = iOr (iOffset z (-1) dflt) (iOffset y 1 dflt)

dflt = iValBool False

-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm x
       , toDynStrm y
       , toDynStrm z]
