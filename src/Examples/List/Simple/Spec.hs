module Examples.List.Simple.Spec (spec) where

import MCLola
import Theories.Numeric
import Theories.List

{-  Specification
    
 -- Input
 
 input double p
 input double q
 
 -- Output
 
 output [double] l1 = [p, 0.7, q[2|0.7]]
 output [double] l2 = l1                 -- same as l1 but using foldr
 output [double] l3 = l1 ++ l2[1|l2]
 
 output bool c1               = elem 1.5 l1
 output [double] c2           = filterElem [1.5,1.2,1.9] l3
 output [(double, double)] c3 = zip l1 l3
 
-}

-- | Input

p :: Stream Double
p = In "p"

q :: Stream Double
q = In "q"

-- | Output

l1 :: Stream [Double]
l1 = Out ("l1", expr)
  where
    e1   = iNow p
    e2   = iValDouble 0.7
    e3   = iOffset q 2 (iValDouble 0.7)
    expr = iConsL e1 (iConsL e2 (iSinglL e3)) 

l2 :: Stream [Double]
l2 = Out ("l2", expr)
  where
    e1   = iNow p
    e2   = iValDouble 0.7
    e3   = iOffset q 2 (iValDouble 0.7)
    expr = foldr iConsL (iEmptyL e1) [e1, e2, e3]

l3 :: Stream [Double]
l3 = Out ("l3", expr)
  where
    expr = concatL (iNow l1) (iOffset l2 1 (iNow l2))

c1 :: Stream Bool
c1 = Out ("c1", expr)
  where
    expr = elemL (iValDouble 1.5) (iNow l1)
    
c2 :: Stream [Double]
c2 = Out ("c2", expr)
  where
    e1   = iValDouble 1.5
    e2   = iValDouble 1.2
    e3   = iValDouble 1.9
    l    = foldr iConsL (iSinglL e3) [e1, e2]
    expr = filterElem (iValDouble 2) l (iNow l3)
    
c3 :: Stream [(Double, Double)]
c3 = Out ("c3", expr)
  where
    expr = zipL (iNow l1) (iNow l3)

-- | Specification

spec :: Specification
spec = [ toDynStrm p
       , toDynStrm q 
       , toDynStrm l1, toDynStrm l2, toDynStrm l3
       , toDynStrm c1, toDynStrm c2, toDynStrm c3 ]
