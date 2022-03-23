module Examples.Geometry.PointInPoly.Spec (spec) where

import MCLola
import Theories.Numeric
import Theories.Geometry2D
import Theories.List

{-  Specification
    
 -- Input
 
 input double p
 input double q
 
 -- Output
 
 define point2 p1 = (p, q)
 define point2 p2 = (p[1|0.9], q)
 define point2 p3 = (p, q[1|0.9])
 
 output [point2] poly = [p1, p2, p3]
 output bool pInPoly  = pointInPoly (p, q) poly
 output bool pInPoly2 = pointInPoly (0, 0) poly
 
-}

-- | Input

p :: Stream Double
p = In "p"

q :: Stream Double
q = In "q"

-- | Output

poly :: Stream [Point2]
poly = Out ("poly", expr)
  where
    p1 = iPoint2D (iNow p) (iNow q)
    p2 = iPoint2D (iOffset p 1 (iValDouble 0.9)) (iNow q)
    p3 = iPoint2D (iNow p) (iOffset q 1 (iValDouble 0.9))
    expr = foldr iConsL (iSinglL p3) [p1, p2]

pInPoly :: Stream Bool
pInPoly = Out ("pInPoly", expr)
  where
    expr = pointInPoly (iPoint2D (iNow p) (iNow q)) (iNow poly)
    
pInPoly2 :: Stream Bool
pInPoly2 = Out ("pInPoly2", expr)
  where
    expr = pointInPoly (iPoint2D (iValDouble 0) (iValDouble 0)) (iNow poly)

-- | Specification

spec :: Specification
spec = [ toDynStrm p
       , toDynStrm q
       , toDynStrm poly
       , toDynStrm pInPoly
       , toDynStrm pInPoly2 ]
