module StaticAnalysis.StaticAnalysis (analysis, outputAnalysis) where

import Language.Spec
import Internal.TypeRepr
import Internal.Maps
import StaticAnalysis.TypesComputation  (computeTypes, createTypeMap)
import StaticAnalysis.TempRef           (computeTempRef)

import Data.Map                         (Map, lookup, toList, empty, insert, insertWith, unionWith, foldr, foldrWithKey, keys, filter, fromList)
import Data.List                        (maximum, minimum, (\\), nub)
import Data.Maybe                       (fromMaybe, catMaybes)
import Prelude                          hiding (lookup)

{-

  Static Analysis
  
  The static analysis is responsible of:
  - Check well-formed and future-bounded.
  - Determine buffer information for each stream (size and type).
  - Compute all types.
  - Compute latency for each stream.
  - Order specification's streams according to temporal dependencies, so that streams can be evaluated in a correct order.

-}


-- || Check spec for stream name repetition

checkNames :: Specification -> Bool
checkNames spec = length names == length (nub names)
  where
    names = map getIdent spec


-- || Dependency graph represented with adjacency list

type Vert   = String
type DGraph = Map Vert (Map Vert [Int])

addEdges :: DynStream -> DGraph -> DGraph
addEdges (DIn _, _) g       = g
addEdges (DOut (s, e), _) g = insertWith (unionWith (++)) s (newEdges e) g
  where
    newEdges e = Prelude.foldr (\(id, k) -> insertWith (+++) id [k]) empty (computeTempRef e)
    xs +++ ys  = xs ++ (Prelude.filter (\y -> not (elem y xs)) ys)

createDGraph :: Specification -> DGraph
createDGraph spec = Prelude.foldr addEdges emptyGraph spec
  where
    addVert s  = insert (getIdent s) empty
    emptyGraph = Prelude.foldr addVert empty spec


-- || Check well-formed and future-bounded

-- Check if a given specification is well-formed and future-bounded,
-- ie: returns false iff dgraph contains a cycle with weight greater or equal to 0

checkDGraph :: DGraph -> Bool
checkDGraph dg = checkVerts (keys dg) dg

checkVerts :: [Vert] -> DGraph -> Bool
checkVerts [] _      = True
checkVerts (v:vs) dg = if (checkVert v dg) then checkVerts vs dg else False

checkVert :: Vert -> DGraph -> Bool
checkVert v dg = checkVertC v (neighbours v dg) dg

neighbours :: Vert -> DGraph -> [Vert]
neighbours v dg = keys (fromMaybe empty (lookup v dg))

checkVertC :: Vert -> [Vert] -> DGraph -> Bool
checkVertC v [] _      = True
checkVertC v (n:ns) dg = case (maxCycle v [n] (maxW v n dg) dg) of
                           Nothing -> checkVertC v ns dg
                           Just x  -> if (x >= 0) then False else checkVertC v ns dg

maxW :: Vert -> Vert -> DGraph -> Int
maxW v u dg = case (lookup v dg) of
                Nothing -> error "err"
                Just mp -> maximum (fromMaybe (error "err") (lookup u mp))
                
maxCycle :: Vert -> [Vert] -> Int -> DGraph -> Maybe Int
maxCycle v vs w dg | l == v    = Just w
                   | otherwise = if (ns == [])
                                 then Nothing
                                 else maxC (map (\u -> maxCycle v (vs++[u]) (w + (maxW l u dg)) dg) ns)
  where
    l  = last vs
    ns = (neighbours l dg) \\ vs

maxC :: [Maybe Int] -> Maybe Int
maxC rs = if (ws == []) then Nothing else (Just (maximum ws))
  where ws = catMaybes rs


-- || Back-Off Reference computation

createBackRef :: DGraph -> RefMap
createBackRef dg = Data.Map.foldr upd initMap dg
  where
    initMap       = fromList (map (\v -> (v, 0)) (keys dg))
    upd m map     = foldrWithKey updK map m 
    updK k ws map = if (oldmax < maxloc) then insert k maxloc map else map
      where 
        oldmax = fromMaybe (error "err") (lookup k map)
        maxloc = (-1) * (minimum ws)


-- || Latency computation

createLatMap :: DGraph -> LatMap
createLatMap dg = fromList (map (\s -> (s, look_ahead dg s)) (keys dg))

look_ahead :: DGraph -> Vert -> Int
look_ahead dg v = maximum (0:ws)
  where
    ns = (neighbours v dg) \\ [v]
    ws = map (\n -> look_ahead' dg [v,n] (maxW v n dg)) ns

look_ahead' :: DGraph -> [Vert] -> Int -> Int
look_ahead' dg path w = maximum (w:ws)
  where
    l  = last path
    ns = (neighbours l dg) \\ path
    ws = map (\n -> look_ahead' dg (path++[n]) (w + maxW l n dg)) ns


-- || Buffer size computation
   
computeSize :: RefMap -> LatMap -> Integer
computeSize bR lm = toInteger $ maxLat + maxBR + 1
  where
    maxLat = Data.Map.foldr max 0 lm
    maxBR  = Data.Map.foldr max 0 bR


-- || Order specification

-- Order graph
-- Each vertex is mapped with the list of its neighbours and its entry degree

type OrdGraph = Map Vert ([Vert], Int)

order :: Specification -> LatMap -> Specification
order spec lm = map getSpec ordIdent
  where
    g          = createOrdGraph spec lm
    ordIdent   = topSort g
    specMap    = Prelude.foldr (\s -> insert (getIdent s) s) empty spec
    getSpec id = fromMaybe (error "err") (lookup id specMap)

addOrdEdges :: LatMap -> DynStream -> OrdGraph -> OrdGraph
addOrdEdges _ (DIn _, _) g        = g
addOrdEdges lm (DOut (s, e), _) g = Prelude.foldr (\v -> (insertWith comb s ([], 1)) . (insertWith comb v ([s], 0))) g (filterVert s (filterMax (computeTempRef e)) lm)
  where
    comb (vs, n) (us, m) = (vs++us, n+m)

filterMax :: [(Vert, Int)] -> [(Vert, Int)]
filterMax []          = []
filterMax ((v, k):xs) = (v, kmax) : ys
  where
    ys   = Prelude.filter (\(u, _) -> u /= v) xs
    ks   = map snd $ Prelude.filter (\(u, _) -> u == v) xs
    kmax = Prelude.foldr max k ks

filterVert :: Vert -> [(Vert, Int)] -> LatMap -> [Vert]
filterVert s vs lm = map fst $ Prelude.filter (\(v, k) -> (getLat s lm) - (getLat v lm) == k) vs -- <=
 
createOrdGraph :: Specification -> LatMap -> OrdGraph
createOrdGraph spec lm = Prelude.foldr (addOrdEdges lm) emptyGraph spec
  where
    addVert s  = insert (getIdent s) ([], 0)
    emptyGraph = Prelude.foldr addVert empty spec
    
topSort :: OrdGraph -> [Vert]
topSort g = reverse (topSort' zlist nzlist [])
  where
    zlist  = toList (Data.Map.filter (\(_, n) -> n == 0) g)
    nzlist = Data.Map.filter (\(_, n) -> n /= 0) g

topSort' :: [(Vert, ([Vert], Int))] -> OrdGraph -> [Vert] -> [Vert]
topSort' [] nzlist ordlist                   = if (null nzlist) then ordlist else error "not well formed"
topSort' ((v, (vs, _)) : zs) nzlist ordlist  = topSort' (zs ++ newzs) newnzs (v : ordlist)
  where
    comb (vs, n) (us, m) = (vs++us, n+m)
    g      = Prelude.foldr (\w -> insertWith comb w ([], -1)) nzlist vs
    newzs  = toList (Data.Map.filter (\(_, n) -> n == 0) g)
    newnzs = Data.Map.filter (\(_, n) -> n /= 0) g


-- || Static Analysis
    
analysis :: Specification -> (DGraph, LatMap, Integer, Specification, TypeMap, [TypeRepr], [Ident])
analysis spec | not (checkNames spec)    = error "Repeated stream names in specification"
              | not (checkDGraph dgraph) = error "Error in specification: not well-formed or not future-bounded"
              | otherwise                = (dgraph, latMap, size, ordSpec, typeMap, types, input)
  where
    -- Compute dependency graph
    dgraph  = createDGraph spec
    
    -- Compute backRef
    backRef = createBackRef dgraph
    
    -- Compute latency
    latMap  = createLatMap dgraph
    
    -- Compute buffers size
    size    = computeSize backRef latMap
    
    -- Order specification
    ordSpec = order spec latMap
    
    -- Compute types
    typeMap = createTypeMap spec
    types   = computeTypes spec
    
    -- Input streams
    input   = map getIdent $ Prelude.filter isInput ordSpec


-- || Output Analysis

outputAnalysis :: DGraph -> LatMap -> IO()
outputAnalysis dgraph latMap = writeFile "monitor/analysis.txt" content
  where
    content = unlines
      [ clasif
      , ""
      , "* Dependency graph"
      , foldrWithKey showDG "" dgraph
      , "* Latency"
      , foldrWithKey showLat "" latMap]
      
    clasif = if (Data.Map.foldr max 0 latMap == 0)
             then "* Very Efficiently Monitorable Specification"
             else "* Efficiently Monitorable Specification"
    
    showDG n ns ss = n ++ " ->" ++ format (foldrWithKey showN "" ns) ++ "\n" ++ ss
      where format [] = " -"
            format ss = init ss
            
    showN n w ss   = " " ++ n ++ show w ++ "," ++ ss
    
    showLat n l ss = "latency(" ++ n ++ ") = " ++ show l ++ "\n" ++ ss
