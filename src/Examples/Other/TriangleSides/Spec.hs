module Examples.Other.TriangleSides.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric
import Theories.Either
import Theories.Maybe
import Theories.List

{-  Specification
    
 -- Input
 
 input double s1
 input double s2
 input double s3
 
 -- Output
 
 output (either double string) per =
   if !(s1 > 0 /\ s2 > 0 /\ s3 > 0)
   then right "Sides have to be positive"
   else if (s1 >= s2+s3)
        then right "s1 >= s2 + s3"
        else if (s2 >= s1+s3)
             then right "s2 >= s1 + s3"
             else if (s3 >= s1+s2)
                  then right "s3 >= s1 + s2"
                  else left (s1+s2+s3)
 
 output (maybe double) avgSide = 
   if (isLeaf per)
   then just ((fromLeaf per) / 3)
   else nothing
  
-}

-- | Input

s1 :: Stream Double
s1 = In "s1"

s2 :: Stream Double
s2 = In "s2"

s3 :: Stream Double
s3 = In "s3"

-- | Output

per :: Stream (Either Double String)
per = Out ("per", expr)
  where
    l1 = iNow s1
    l2 = iNow s2
    l3 = iNow s3
    zr = iValDouble 0
    
    m0 = foldr (\ c -> iConsL (iValChar c)) (iSinglL (iValChar '.')) "Sides have to be positive"
    m1 = foldr (\ c -> iConsL (iValChar c)) (iSinglL (iValChar '.')) "s1 >= s2 + s3"
    m2 = foldr (\ c -> iConsL (iValChar c)) (iSinglL (iValChar '.')) "s2 >= s1 + s3"
    m3 =  foldr (\ c -> iConsL (iValChar c)) (iSinglL (iValChar '.')) "s3 >= s1 + s2"

    c0 = iNot $ iAnd (iDGt l1 zr) (iAnd (iDGt l2 zr) (iDGt l3 zr))
    c1 = iDGeq l1 (iDAdd l2 l3)
    c2 = iDGeq l2 (iDAdd l1 l3)
    c3 = iDGeq l3 (iDAdd l1 l2)

    expr = iIte c0
             (iEitherR zr m0)
             (iIte c1
               (iEitherR zr m1)
               (iIte c2
                 (iEitherR zr m2)
                 (iIte c3
                   (iEitherR zr m3)
                   (iEitherL (foldr iDAdd zr [l1, l2, l3]) (iSinglL (iValChar '.'))))))
    
avgSide :: Stream (Maybe Double)
avgSide = Out ("avgSide", expr)
  where
    expr = iIte (iIsLeft (iNow per))
                (iJustM (iDDiv (iFromLeft (iNow per)) (iValDouble 3)))
                (iNothingM (iValDouble 0))

-- | Specification

spec :: Specification
spec = [ toDynStrm s1, toDynStrm s2, toDynStrm s3
       , toDynStrm per, toDynStrm avgSide ]
