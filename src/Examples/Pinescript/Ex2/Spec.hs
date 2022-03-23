module Examples.Pinescript.Ex2.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric
import Theories.Maybe

-- | Input

x :: Stream Double
x = In "x"

-- | Output

instant :: Stream Int
instant = Out("instant", expr)
  where
    expr = iIAdd (iOffset instant (-1) (iValInt 0)) (iValInt 1)

sma5 :: Stream (Maybe Double)
sma5 = Out("sma5", expr)
  where
    num  = foldr (\off e -> iDAdd (iOffset x off (iValDouble 0)) e) (iNow x) [(-4)..(-1)]
    den  = iValDouble 5
    expr = iIte (iDLeq den (iIntToDouble (iNow instant)))
                (iJustM (iDDiv num den))
                (iNothingM (iValDouble 0))
    
-- | Specification

spec :: Specification
spec = [toDynStrm x, toDynStrm instant, toDynStrm sma5]
