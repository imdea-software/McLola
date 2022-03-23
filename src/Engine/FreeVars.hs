{-# LANGUAGE GADTs, TemplateHaskell #-}

module Engine.FreeVars where

import Data.Comp.Multi         (Term, Alg, cata)
import Data.Comp.Multi.Derive  (derive, HFunctor, liftSum)

{-

  Fresh names generation, to avoid free variable capture when using let bindings
  
-}

-- || Free variables computation

data FreeVars e where
  FreeVars :: [String] -> FreeVars e

class FreeVarsAlg e where
  freeVarsAlg :: Alg e FreeVars
  
unFreeVars (FreeVars x) = x

strsIn :: (HFunctor e, FreeVarsAlg e) => Term e a -> [String]
strsIn t = unFreeVars (cata freeVarsAlg t)

-- Derive boilerplate code using Template Haskell
$(derive [liftSum] [''FreeVarsAlg])


-- || Fresh names generation

-- Implemented with a trie structure.

fresh :: Int -> [String] -> [String]
fresh 1 ss = [fresh1 ss]
fresh n ss = let f = fresh1 ss
             in  f : (fresh (n-1) (f:ss))

fresh1 :: [String] -> String
fresh1 ss = newStr (foldr add Null ss)

-- Trie implementation

data Trie = Null | Node [(Bool, Trie)] deriving Show

abc = ['a' .. 'z']

add :: String -> Trie -> Trie
add s t = if (foldr (\ c b -> b || (not (elem c abc))) False s)
          then t
          else addS s t
  
addS :: String -> Trie -> Trie
addS "" t        = t
addS (c:"") Null     = Node $ map (\d -> (c == d, Null)) abc
addS (c:"") (Node t) = Node $ map (\(d, (b, tr)) -> (if (c == d) then True else b, tr)) (zip abc t)
addS (c:s) Null      = Node $ map (\d -> (False, if (c == d) then (addS s Null) else Null)) abc
addS (c:s) (Node t)  = Node $ map (\(d, (b, tr)) -> (b, if (c == d) then addS s tr else tr)) (zip abc t)

newStr :: Trie -> String
newStr t = solve [("", t)]

solve :: [(String, Trie)] -> String
solve []               = error "err"
solve ((s, Null):ss)   = s ++ ['a']
solve ((s, Node t):ss) = case (filter (\(_, (b,_)) -> not b) (zip abc t)) of
  ((c,_):_) -> s ++ [c]
  _         -> solve (ss ++ children)
    where
      children = map (\(c, (_,tr)) -> (s++[c], tr)) (zip abc t)

