module StaticAnalysis.TypesComputation (createTypeMap, computeTypes) where

import Language.Spec
import Internal.TypeRepr
import Internal.Maps
import StaticAnalysis.Types

import Data.Map              (empty)
import Control.Monad.State   (State, runState)
import Data.Map              (fromList) 

{-

  Types Computation
  Given a specification, compute all the types on it (streams types + subexpressions types + subtypes + maybe types)

-}

createTypeMap :: Specification -> TypeMap
createTypeMap spec = fromList (map (\(str, tr) -> (getId str, tr)) spec)

computeTypes :: Specification -> [TypeRepr]
computeTypes spec = (addBool . extMaybe . extTypes . fst) types
  where
    types      = runState (compTypes spec) (createTypeMap spec, empty)
    addBool ts = if (elem TBool ts) then ts else ([TBool, TMaybe TBool] ++ ts)
    
compTypes :: Specification -> State (TypeMap, TypeMap) [TypeRepr]
compTypes []       = return []
compTypes (s:spec) = do tys <- compTypes spec
                        ts  <- streamTypes s
                        return $ merge ts tys
  where
    merge [] ts     = ts
    merge (x:xs) ts = if (elem x ts) then merge xs ts else merge xs (x:ts)


streamTypes :: DynStream -> State (TypeMap, TypeMap) [TypeRepr]
streamTypes (DIn _, t)       = return [t]
streamTypes (DOut (_, e), _) = do (ts, _) <- exprTypes e
                                  return ts

-- Given a list of types, computes all subtypes in appropiate order for compilation

extTypes :: [TypeRepr] -> [TypeRepr]
extTypes ts = extend ts []
  where
    extend [] tys = reverse tys
  
    extend ((t@(TMaybe t1)):typs) tys
      | elem t tys  = extend typs tys
      | elem t1 tys = extend typs (t:tys)
      | otherwise   = extend ([t1] ++ (t:typs)) tys
        
    extend ((t@(TEither t1 t2)):typs) tys 
      | elem t tys                 = extend typs tys
      | elem t1 tys && elem t2 tys = extend typs (t:tys)
      | otherwise                  = extend ([t1, t2] ++ (t:typs)) tys
        
    extend ((t@(TList t1)):typs) tys 
      | elem t tys  = extend typs tys
      | elem t1 tys = extend typs (t:tys)
      | otherwise   = extend ([t1] ++ (t:typs)) tys
        
    extend ((t@(TProd t1 t2)):typs) tys 
      | elem t tys                 = extend typs tys
      | elem t1 tys && elem t2 tys = extend typs (t:tys)
      | otherwise                  = extend ([t1, t2] ++ (t:typs)) tys
        
    extend ((t@(TNAProd n fs)):typs) tys 
      | elem t tys                                            = extend typs tys
      | foldr (\ty p -> p && (elem ty tys)) True (map snd fs) = extend typs (t:tys)
      | otherwise                                             = extend ((map snd fs) ++ (t:typs)) tys
        
    extend (t:typs) tys 
      | elem t tys = extend typs tys
      | otherwise  = extend typs (t:tys)


-- Adds Maybe types

extMaybe :: [TypeRepr] -> [TypeRepr]
extMaybe types = auxAddMaybe types types
  where
    auxAddMaybe ts types = foldl (\tys t -> if (TMaybe t) `elem` tys then tys else (tys++[TMaybe t])) types ts

