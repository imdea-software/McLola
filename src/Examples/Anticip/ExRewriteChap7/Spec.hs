module Examples.Anticip.ExRewriteChap7.Spec (spec) where

import MCLola
import Theories.Numeric
import Theories.Product

-- | Input

a :: Stream Int
a = In "a"

-- | Output

x :: Stream Int
x = Out ("x", expr)
  where
    expr = iFst pair
    pair = iPair (iOffset a (-1) (iValInt 5)) (iOffset a 3 (iValInt 5))
    
-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm x ]
