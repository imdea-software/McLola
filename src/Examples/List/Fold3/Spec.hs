module Examples.List.Fold3.Spec (spec) where

import MCLola
import Theories.Numeric
import Theories.List

{-  Specification
    
 -- Input
 
 input double p
 input double q
 
 -- Output
 
 output [double] ex1 = foldr cons 0 [p[1|0.2]] [p, q]
 
 output [[double]] ex2 = [[3,2,1], ex1, ex1[1|[1,2,3]], ex1[-1|[1,2,3]]]
 
 output double ex3 = foldr + 0 (foldr concat (head ex2) (tail ex2))
 
-}

-- | Input

p :: Stream Double
p = In "p"

q :: Stream Double
q = In "q"

-- | Output

ex1 :: Stream [Double]
ex1 = Out ("ex1", expr)
  where
    l1   = foldr iConsL (iSinglL (iNow q)) [iNow p]
    expr = foldrL iConsL (iSinglL (iOffset p 1 (iValDouble 0.2))) l1
    
ex2 :: Stream [[Double]]
ex2 = Out ("ex2", expr)
  where
    l0   = foldr (\ c -> iConsL (iValDouble c)) (iSinglL (iValDouble 3)) [1, 2]
    l1   = foldr (\ c -> iConsL (iValDouble c)) (iSinglL (iValDouble 1)) [3, 2]
    l2   = iNow ex1
    l3   = iOffset ex1 1 l0
    l4   = iOffset ex1 (-1) l0
    expr = foldr iConsL (iEmptyL l0) [l1, l2, l3, l4]
    
ex3 :: Stream Double
ex3 = Out ("ex3", expr)
  where
    l    = iNow ex2
    empt = iEmptyL (iValDouble 0)
    flat = foldrL concatL empt l
    expr = foldrL iDAdd (iValDouble 0) flat
    
ex4 :: Stream [Double]
ex4 = Out ("ex4", expr)
  where
    empt = iEmptyL (iValDouble 0)
    l    = foldr iConsL empt [iValDouble 0.8, iValDouble 0.6, iValDouble 3.8, iValDouble 4]
    expr = filterElem (iValDouble 0) l (iNow ex1)
    
-- | Specification

spec :: Specification
spec = [ toDynStrm p, toDynStrm q 
       , toDynStrm ex1, toDynStrm ex2, toDynStrm ex3, toDynStrm ex4 ]
