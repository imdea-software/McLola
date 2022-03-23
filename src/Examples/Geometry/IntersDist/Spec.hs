module Examples.Geometry.IntersDist.Spec (spec) where

import MCLola
import Theories.Numeric
import Theories.Geometry2D

{-  Specification
    
 -- Input
 
 input double p
 input double q
 
 -- Output
 
 define point2 p1  = (p, p[1|0.7])
 define vector2 v1 = (q[1|0.7], p[1|0.7])
 define point2 p2  = (q, q[2|0.7])
 define vector2 v2 = (p[-1|0.7], q[1|0.7])
 
 output double idst = intersectionDist p1 v1 p2 v2
 
-}

-- | Input

p :: Stream Double
p = In "p"

q :: Stream Double
q = In "q"

-- | Output

idst :: Stream (Either String Double)
idst = Out ("ex1", expr)
  where
    p1 = iPoint2D (iNow p) (iOffset p 1 (iValDouble 0.7))
    v1 = iPoint2D (iOffset q 1 (iValDouble 0.7)) (iOffset p 1 (iValDouble 0.7))
    p2 = iPoint2D (iNow q) (iOffset q 2 (iValDouble 0.7))
    v2 = iPoint2D (iOffset p (-1) (iValDouble 0.7)) (iOffset q 1 (iValDouble 0.7))
    expr = iIntersDistance p1 v1 p2 v2

-- | Specification

spec :: Specification
spec = [ toDynStrm p
       , toDynStrm q 
       , toDynStrm idst ]
