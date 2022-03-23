module Examples.List.Fold2.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric
import Theories.List

{-  Specification
    
 -- Input
 
 input double p
 input double q
 input bool r
 input int s
 
 -- Output

 output int ex1 = foldr (\ p a -> if p then a+1 else a) 0 [r[-2|false], r, r[1|false], r[3|false], true]
 
 output int ex2 = foldr + 0 [s[-2|0], s, s[1|0], s3|0], 1]
 
 output bool ex3 = foldr (\ d b -> if d >= -1.2 then b false) true [p, q, p[1|0], q[1|0]]
 
-}

-- | Input

p :: Stream Double
p = In "p"

q :: Stream Double
q = In "q"

r :: Stream Bool
r = In "r"

s :: Stream Int
s = In "s"

-- | Output
    
ex1 :: Stream Int
ex1 = Out ("ex1", expr)
  where
    false = iValBool False
    l     = foldr iConsL (iSinglL (iValBool True)) [iOffset r (-2) false, iNow r, iOffset r 1 false, iOffset r 3 false]
    fun   = \ b c -> iIte b (iIAdd c (iValInt 1)) c
    expr  = foldrL fun (iValInt 0) l
    
ex2 :: Stream Int
ex2 = Out ("ex2", expr)
  where
    zero  = iValInt 0
    l     = foldr iConsL (iSinglL (iValInt 1)) [iOffset s (-2) zero, iNow s, iOffset s 1 zero, iOffset s 3 zero]
    fun   = iIAdd
    expr  = foldrL fun zero l

ex3 :: Stream Bool
ex3 = Out ("ex3", expr)
  where
    zero = iValDouble 0
    empt = iEmptyL zero
    l    = foldr iConsL empt [iNow p, iNow q, iOffset p 1 zero, iOffset q 1 zero]
    fun  = \ d b -> iIte (iDGeq d (iValDouble (-1.2))) b (iValBool False)
    expr = foldrL fun (iValBool True) l

-- | Specification

spec :: Specification
spec = [ toDynStrm p, toDynStrm q , toDynStrm r, toDynStrm s
       , toDynStrm ex1, toDynStrm ex2, toDynStrm ex3 ]
