module Examples.List.Fold1.Spec (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric
import Theories.List

{-  Specification
    
 -- Input
 
 input double p
 input double q
 input bool r
 
 -- Output
 
 output double ex1 = foldr + 0 [p, q, p[1|1], q[1|1]]
 
 output bool ex2 = foldr or false [r, r[1|false], r[3|false], r[-2|false]] 
 
-}

-- | Input

p :: Stream Double
p = In "p"

q :: Stream Double
q = In "q"

r :: Stream Bool
r = In "r"

-- | Output
    
ex1 :: Stream Double
ex1 = Out ("ex1", expr)
  where
    one   = iValDouble 1
    empty = iEmptyL one
    l     = foldr iConsL empty [iNow p, iNow q, iOffset p 1 one, iOffset q 1 one]
    expr  = foldrL iDAdd (iValDouble 0) l

ex2 :: Stream Bool
ex2 = Out ("ex2", expr)
  where
    false = iValBool False
    empty = iEmptyL false
    l     = foldr iConsL empty [iNow r, iOffset r 1 false, iOffset r 3 false, iOffset r (-2) false]
    expr  = foldrL iOr false l
    
-- | Specification

spec :: Specification
spec = [ toDynStrm p, toDynStrm q, toDynStrm r
       , toDynStrm ex1, toDynStrm ex2 ]
