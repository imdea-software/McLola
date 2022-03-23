module Examples.Pinescript.Ex4.Spec (spec) where

import MCLola
import Lib.Pinescript

-- | Input

x :: Stream Double
x = In "x"

y :: Stream Double
y = In "y"

-- | Output

sma5x :: Stream Double
sma5x = sma x 5
    
sma5y :: Stream Double
sma5y = sma y 5

crov :: Stream Bool
crov = crossover sma5x sma5y

-- | Specification

spec :: Specification
spec = [ toDynStrm x, toDynStrm y
       , toDynStrm instant, toDynStrm sma5x, toDynStrm sma5y
       , toDynStrm crov]
