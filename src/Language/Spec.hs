{-# Language GADTs #-}

module Language.Spec (Specification, DynStream, DynStrm(..), getId, getIdent, getTypeRepr, isInput, toDynStrm) where

import Language.Lola
import Internal.TypeRepr

import Type.Reflection    (typeOf)

{-

  MCLola Specifications
  (Type unification for MCLola streams)

-}

-- || Specification Type

type Specification = [DynStream]


-- || Streams with unified type

type DynStream = (DynStrm, TypeRepr)

data DynStrm where
  DIn  :: Identifier          -> DynStrm
  DOut :: (Identifier, Exp a) -> DynStrm

getId :: DynStrm -> Identifier
getId (DIn s)       = s
getId (DOut (s, _)) = s

getIdent :: DynStream -> Identifier
getIdent (s, _) = getId s

getTypeRepr :: DynStream -> TypeRepr
getTypeRepr (_, t) = t

isInput :: DynStream -> Bool
isInput (DIn _, _) = True
isInput _          = False


-- || Translation from Stream to DynStream

toDynStrm :: Streamable a => Stream a -> DynStream
toDynStrm x@(In s)       = (DIn s, streamType x)
toDynStrm x@(Out (s, e)) = (DOut (s, e), streamType x)

-- | Type computation

streamType :: Streamable a => Stream a -> TypeRepr
streamType = parseTyp . (drop 7) . show . typeOf

-- Parser - basic types
parseTyp "Int"    = TInt
parseTyp "Double" = TDouble
parseTyp "Bool"   = TBool
parseTyp "Char"   = TChar

-- Parser - record types
parseTyp "Point2"   = TNAProd "Point2" [("x", TDouble), ("y", TDouble)]
parseTyp "Attitude" = TNAProd "Attitude" [("yaw", TDouble), ("roll", TDouble), ("pitch", TDouble)]
parseTyp "Position" = TNAProd "Position" [("xp", TDouble), ("yp", TDouble), ("alt", TDouble), ("zone", TDouble)]
parseTyp "Target"   = TNAProd "Target" [("xt", TDouble), ("yt", TDouble), ("mutex", TDouble), ("num_wp", TDouble)]

-- Parser - other types
-- List
parseTyp x | take 1 x == "["       = TList (parseTyp (init (tail x)))
-- Either
           | take 6 x == "Either"  = let (l,r) = splitE (drop 7 x) in TEither (parseTyp l) (parseTyp r)
           | take 7 x == "(Either" = let (l,r) = splitE (drop 8 (init x)) in TEither (parseTyp l) (parseTyp r)
-- Maybe
           | take 6 x == "(Maybe"  = TMaybe (parseTyp (drop 7 (init x)))
           | take 5 x == "Maybe"   = TMaybe (parseTyp (drop 6 x))
-- Product
           | take 1 x == "("       = let (l,r) = splitP x in TProd (parseTyp l) (parseTyp r)
-- Type not supported
           | otherwise             = error ("Type " ++ x ++ " not supported.")

splitE xs = aux xs [] 0 0
  where
    aux ('(':r) l n m = aux r ('(':l) (n+1) m 
    aux (')':r) l n m = aux r (')':l) n (m+1) 
    aux (' ':r) l n m = if (n == m) then (reverse l, r) else aux r (' ':l) n m
    aux (c:r)   l n m = aux r (c:l) n m
                             
splitP xs = aux ((init.tail) xs) 0 0 []
  where
    aux (',':r) n m l = if (n == m) then (l,r) else aux r n m (l++[','])
    aux ('(':r) n m l = aux r (n+1) m (l++['('])
    aux (')':r) n m l = aux r n (m+1) (l++[')'])
    aux (x:r)   n m l = aux r n m (l++[x])
    aux _       n m l = error ("Type " ++ xs ++ " not supported.")
