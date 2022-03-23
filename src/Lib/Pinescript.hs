module Lib.Pinescript where

import MCLola
import Theories.Bool
import Theories.Numeric

{-

  Pine Script library

-}

-- | Instant stream

instant :: Stream Int
instant = Out("instant", expr)
  where
    expr = iIAdd (iOffset instant (-1) (iValInt 0)) (iValInt 1)

-- | Simple Moving Average (needs instant)

sma :: Stream Double -> Int -> Stream Double
sma x n = Out(name, expr)
  where
    name = "sma_" ++ show n ++ "_" ++ ident x
    num  = foldr (\off e -> iDAdd (iOffset x off (iValDouble 0)) e) (iNow x) [((-1)*(n-1))..(-1)]
    den  = iIntToDouble (iIMin (iNow instant) (iValInt n))
    expr = iDDiv num den

-- | Crossover

crossover :: Stream Double -> Stream Double -> Stream Bool
crossover x y = Out (name, expr)
  where
    name = ident x ++ "_crossover_" ++ ident y
    dflt = iValDouble 0
    expr = iAnd (iDLeq (iNow y) (iNow x))
                (iDLeq (iOffset x (-1) dflt) (iOffset y (-1) dflt))
