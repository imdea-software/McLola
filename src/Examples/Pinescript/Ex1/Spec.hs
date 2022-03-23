module Examples.Pinescript.Ex1.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric

-- | Input

x :: Stream Double
x = In "x"

y :: Stream Double
y = In "y"

-- | Output

instant :: Stream Int
instant = Out("instant", expr)
  where
    expr = iIAdd (iOffset instant (-1) (iValInt 0)) (iValInt 1)

sma5x :: Stream Double
sma5x = Out("sma5x", expr)
  where
    num  = foldr (\off e -> iDAdd (iOffset x off (iValDouble 0)) e) (iNow x) [(-4)..(-1)]
    den  = iIntToDouble (iIMin (iNow instant) (iValInt 5))
    expr = iDDiv num den
    
sma5y :: Stream Double
sma5y = Out("sma5y", expr)
  where
    num  = foldr (\off e -> iDAdd (iOffset y off (iValDouble 0)) e) (iNow y) [(-4)..(-1)]
    den  = iIntToDouble (iIMin (iNow instant) (iValInt 5))
    expr = iDDiv num den

crov :: Stream Bool
crov = Out ("crov", expr)
  where
    dflt = iValDouble 0
    expr = iAnd (iDLeq (iNow sma5y) (iNow sma5x))
                (iDLeq (iOffset sma5x (-1) dflt) (iOffset sma5y (-1) dflt))

-- | Specification

spec :: Specification
spec = [ toDynStrm x, toDynStrm y
       , toDynStrm instant, toDynStrm sma5x, toDynStrm sma5y
       , toDynStrm crov]
