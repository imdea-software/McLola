module Examples.Ex4_2.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric

-- | Input

a :: Stream Int
a = In "a"

-- | Output

x :: Stream Bool
x = Out ("x", expr)
  where
    expr = iOr e1 e2
    e1   = iIGt (iNow a) (iOffset a (-1) (iValInt 0))
    e2   = iOffset x (-1) (iValBool False)

y :: Stream Int
y = Out ("y", expr)
  where
    expr = iIAdd (iOffset y (-1) dflt) (iOffset a 1 dflt)
    dflt = iValInt 1
    
-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm x
       , toDynStrm y]
