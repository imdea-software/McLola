module Examples.Geometry.PolySides.Spec (spec) where

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
 define point2 p4 = (p[1|0.9], q[1|0.9])
 
 output [point2] poly                = [p1, p2, p3, p4]
 output [(point2, point2)] polySides = polySides poly
 
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
    p4 = iPoint2D (iOffset p 1 (iValDouble 0.9)) (iOffset q 1 (iValDouble 0.9))
    expr = foldr iConsL (iSinglL p4) [p1, p2, p3]
    
polySides :: Stream [(Point2, Point2)]
polySides = Out ("polySides", expr)
  where
    expr = polygonSides (iNow poly)

-- | Specification

spec :: Specification
spec = [ toDynStrm p
       , toDynStrm q 
       , toDynStrm poly
       , toDynStrm polySides ]
