module Internal.Maps where

import Internal.TypeRepr  (TypeRepr)

import Data.Map           (Map, lookup, insert)
import Prelude            hiding (lookup)
import Data.Maybe         (fromMaybe)

{-

  Internal structures used for static analysis and translation to C99

-}

type Ident = String

-- Maps the name of a stream with its type representation
type TypeMap = Map Ident TypeRepr

-- Maps the name of a stream with its latency
type LatMap = Map Ident Int

-- Maps the name of a stream with its back-off reference
type RefMap = Map Ident Int

-- Maps the name of reference (for let bindings) with its type representation and variable name used for implementation (uses a list to support repetition of names in nested let bindings)
type BindMap = Map Ident [(TypeRepr, Ident)]


-- | Utils functions

-- TypeMap and LatMap functions

getType :: Ident -> TypeMap -> TypeRepr
getType b m = fromMaybe (error ("Undefined stream " ++ b)) (lookup b m)

getLat :: Ident -> LatMap -> Int
getLat b m = fromMaybe (error ("Undefined stream " ++ b)) (lookup b m)

-- BindMap functions

deleteS :: Ident -> BindMap -> BindMap
deleteS s bm = case lookup s bm of
                 Nothing -> bm
                 Just bs -> insert s (tail bs) bm

insertS :: Ident -> (TypeRepr, Ident) -> BindMap -> BindMap
insertS s (ty, nm) bm = case lookup s bm of
                          Nothing -> insert s [(ty, nm)] bm
                          Just bs -> insert s ((ty, nm):bs) bm

lookupS :: Ident -> BindMap -> Maybe (TypeRepr, Ident)
lookupS s bm = (lookup s bm) >>= Just . head
