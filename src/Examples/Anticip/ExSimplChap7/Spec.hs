module Examples.Anticip.ExSimplChap7.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric

-- | Input

s :: Stream Bool
s = In "s"

a :: Stream Bool
a = In "a"

b :: Stream Bool
b = In "b"

-- | Output

count :: Stream Int
count = Out ("count", expr)
  where
    expr = iIAdd prev curr
    prev = iOffset count (-1) (iValInt 0)
    curr = iIte (iNow ok) (iValInt 1) (iValInt 0)

end :: Stream Bool
end = Out ("end", expr)
  where
    expr = iIGeq (iNow count) (iValInt 3)
    
ok :: Stream Bool
ok = Out ("ok", expr)
  where
    expr = iIte cond e1 e2
    cond = iOr (iOffset s (-2) (iValBool False)) (iOffset s 2 (iValBool False))
    e1   = iAnd (iNow a) (iOffset a 1 (iValBool True))
    e2   = iOr (iNow b) (iOffset b 7 (iValBool True))
    
-- | Specification

spec :: Specification
spec = [ toDynStrm s
       , toDynStrm a
       , toDynStrm b
       , toDynStrm count
       , toDynStrm end
       , toDynStrm ok ]
