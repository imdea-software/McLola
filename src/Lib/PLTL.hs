module Lib.PLTL where

import MCLola
import Theories.Bool

{-

  Past Linear Temporal Logic library

-}

-- | Classic Operators

infix 2 .&&.
(.&&.) :: Stream Bool -> Stream Bool -> Stream Bool
p .&&. q = Out (name, expr)
  where
    name = ident p ++ "_and_" ++ ident q
    expr = iAnd (iNow p) (iNow q)

infix 2 .||.
(.||.) :: Stream Bool -> Stream Bool -> Stream Bool
p .||. q = Out (name, expr)
  where
    name = ident p ++ "_or_" ++ ident q
    expr = iOr (iNow p) (iNow q)

infix 2 .->.
(.->.) :: Stream Bool -> Stream Bool -> Stream Bool
p .->. q = Out (name, expr)
  where
    name = ident p ++ "_impl_" ++ ident q
    expr = iImpl (iNow p) (iNow q)

not :: Stream Bool -> Stream Bool
not p = Out (name, expr)
  where
    name = "not_" ++ ident p
    expr = iNot (iNow p)

-- | Past temporal Operators

yesterday :: Stream Bool -> Stream Bool
yesterday s = Out ("yesterday_" ++ ident s, expr)
  where
    expr = iOffset s (-1) (iValBool False)
    
zyesterday :: Stream Bool -> Stream Bool
zyesterday s = Out ("zyesterday_" ++ ident s, expr)
  where
    expr = iOffset s (-1) (iValBool True)

once :: Stream Bool -> Stream Bool
once s = Out ("once_" ++ ident s, expr)
  where
    expr = iOr (iOffset (once s) (-1) (iValBool False)) (iNow s)

historically :: Stream Bool -> Stream Bool
historically s = Out ("historically_" ++ ident s, expr)
  where
    expr = iAnd (iOffset (historically s) (-1) (iValBool True)) (iNow s)

since :: Stream Bool -> Stream Bool -> Stream Bool
since p q = Out (ident p ++ "_since_" ++ ident q, expr)
  where
    expr = iOr (iNow q) (iAnd (iNow p) (iOffset (since p q) (-1) (iValBool False)))
