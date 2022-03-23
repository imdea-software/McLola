module Examples.Pinescript.Ex3.Spec (spec) where

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

sma :: Stream Double -> Int -> Stream Double
sma x n = Out(name, expr)
  where
    name = "sma_" ++ show n ++ "_" ++ ident x
    num  = foldr (\off e -> iDAdd (iOffset x off (iValDouble 0)) e) (iNow x) [((-1)*(n-1))..(-1)]
    den  = iIntToDouble (iIMin (iNow instant) (iValInt n))
    expr = iDDiv num den

crossover :: Stream Double -> Stream Double -> Stream Bool
crossover x y = Out (name, expr)
  where
    name = ident x ++ "_crossover_" ++ ident y
    dflt = iValDouble 0
    expr = iAnd (iDLeq (iNow y) (iNow x))
                (iDLeq (iOffset x (-1) dflt) (iOffset y (-1) dflt))

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
