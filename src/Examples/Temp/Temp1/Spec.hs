module Examples.Temp.Temp1.Spec (spec) where

import MCLola
import Theories.Numeric

{-  Specification
    
 -- Input
 
 input int c
 
 -- Output
 
 output int a = b[-2|0]
 output int b = c[5|0]
  
-}

-- | Input

c :: Stream Int
c = In "c"

-- | Output

a :: Stream Int
a = Out ("a", expr)
  where
    expr = iOffset b (-2) dflt
    dflt = iValInt 0

b :: Stream Int
b = Out ("b", expr)
  where
    expr = iOffset c 5 dflt
    dflt = iValInt 0

-- | Specification

spec :: Specification
spec = [ toDynStrm a
       , toDynStrm b
       , toDynStrm c ]
