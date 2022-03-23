module Engine.Engine (compileEngine) where
  
import Language.Spec
import Engine.Trans              (transDef, transSim)
import Internal.C99              (toC99Type)
import Internal.Maps             (LatMap, TypeMap, getLat, getType)
import Internal.TypeRepr         (TypeRepr(..))

import Language.C99.Simple
import Language.C99.Pretty       (pretty)
import Text.PrettyPrint          (render)
import Prelude                   hiding (LT)
import Data.Map                  (keys, toList, empty)
import Control.Monad.State.Lazy  (State, evalState)


{-

  C99 code generation for specification engine
  
  The compileEngine function generates two files that implement the 
  engine for the specification:
  
  engine.h: - declaration of initialise, step and finish functions
  engine.c: - implementation of initialise, step and finish functions

-}

-- || Engine declarations

decln :: [Decln]
decln = [initDec,
         stepDec,
         finishDec]
  where
    initDec   = FunDecln Nothing (TypeSpec Void) "initialise" []
    stepDec   = FunDecln Nothing (TypeSpec Void) "step" [Param (Ptr (TypeSpec (TypedefName "input"))) "s"]
    finishDec = FunDecln Nothing (TypeSpec Void) "finish" []


-- || Engine implementation 


-- | Variable declarations

declnc :: TypeMap -> Integer -> [Decln]
declnc tyMap size = [nowDec, inpEndDec] ++ map buffDec (keys tyMap) ++ map arrDec (keys tyMap)
  where
    nowDec    = VarDecln Nothing (TypeSpec Int) "now" Nothing
    inpEndDec = VarDecln Nothing (TypeSpec Int) "inputEnd" Nothing
    buffDec b = VarDecln Nothing (TypeSpec (TypedefName ("buffer" ++ show (getType b tyMap)))) (b ++ "_buff") Nothing
    arrDec b  = VarDecln Nothing (Array (toC99Type (TMaybe (getType b tyMap))) ((Just . LitInt) size)) (b ++ "_arr") Nothing

engineh :: TransUnit
engineh = TransUnit decln []


-- | Time management

moveClock :: [String] -> Integer -> FunDef
moveClock idents size = FunDef typ "moveClock" params decln stms
  where
    typ    = (TypeSpec Void)
    params = []
    decln  = []
    stms   = [Expr (AssignOp AssignAdd (Ident "now") (LitInt 1))] ++
             (map (\s -> Expr (AssignOp Assign (Dot (Index (Dot (Ident (s ++ "_buff")) "arr") (BinaryOp Mod (Ident "now") (LitInt size))) "nothing") (LitInt 1))) idents) ++
             [Return Nothing]

undoClock :: [String] -> Integer -> FunDef
undoClock idents size = FunDef typ "undoClock" params decln stms
  where
    typ    = (TypeSpec Void)
    params = []
    decln  = []
    stms   = (map (\s -> Expr (AssignOp Assign (Dot (Index (Dot (Ident (s ++ "_buff")) "arr") (BinaryOp Mod (Ident "now") (LitInt size))) "nothing") (LitInt 0))) idents) ++
             [Expr (AssignOp AssignSub (Ident "now") (LitInt 1))] ++
             [Return Nothing]


-- | Initialisation function

initBuff :: TypeMap -> Integer -> String -> Stmt
initBuff tyMap size b = Expr (AssignOp Assign (Ident name) (Funcall (Ident ("init" ++ (show ty))) [Ident name, Ident arr, LitInt size]))
  where
    name = b ++ "_buff"
    ty   = getType b tyMap
    arr  = b ++ "_arr"

initialise :: TypeMap -> Integer -> FunDef
initialise tyMap size  = FunDef typ "initialise" params decln stms
  where
    typ    = (TypeSpec Void)
    params = []
    decln  = []
    stms   =  -- Initialise now and endInput
              [ Expr (AssignOp Assign (Ident "now") (LitInt 0))
              , Expr (AssignOp Assign (Ident "inputEnd") (LitInt 0))] ++
              -- Initialise buffers
              map (initBuff tyMap size) (keys tyMap) ++         
              [Return Nothing]
           
-- Auxiliar functions

-- Given the identifier of buffer b and an expression e, pushes expression e in buffer b
save :: TypeMap -> String -> Language.C99.Simple.Expr -> Stmt
save tm b e = Expr (AssignOp Assign (Ident name) (Funcall (Ident ("put" ++ ty)) [Ident name, e, Ident "now"]))
  where
    name = b ++ "_buff"
    ty   = show (getType b tm)

-- Output result
printR :: TypeRepr -> Language.C99.Simple.Expr -> Language.C99.Simple.Expr -> Language.C99.Simple.Expr -> [Stmt]
printR ty i s val = [ Expr (Funcall (Ident "printf") [LitString "(%d, %s, ", i, s])
                    , Expr (Funcall (Ident ("print" ++ show ty)) [val])
                    , Expr (Funcall (Ident "printf") [LitString ")\n"])]


-- | Step funcion

-- Given a stream, produces its corresponding eval function
evalGen :: DynStream -> TypeMap -> LatMap -> FunDef
evalGen stream typMap latMap = FunDef typ name params decln stms
  where
    typ    = TypeSpec Void
    name   = "eval_" ++ getIdent stream
    params = if (isInput stream) then [Param ((toC99Type . getTypeRepr) stream) "val"] else []
    (decln, stms) = genStreamCode stream typMap latMap

genStreamCode :: DynStream -> TypeMap -> LatMap -> ([Decln], [Stmt])

genStreamCode (DIn s, _) tm _ =
  let ty    = getType s tm
      store = if (elem s (keys tm)) then [save tm s (Ident "val")] else []
      print = printR ty (Ident "now") (LitString s) (Ident "val")
  in  ([], store ++ print)
      
genStreamCode (DOut (s, e), _) tm lm =
  let ty         = getType s tm
      lat        = toInteger $ getLat s lm
      transMonad = if (lat == 0) then transDef e else transSim e
      (expr, edecs, estms, _) = evalState transMonad (tm, s, 0, empty)
      
      pos   = BinaryOp Mod (Ident "i") (Dot (Ident (s ++ "_buff")) "size")
      save  = [ Expr (AssignOp Assign (Dot (Index (Dot (Ident (s ++ "_buff")) "arr") pos) "nothing") (LitInt 0))
              , Expr (AssignOp Assign (Dot (Index (Dot (Ident (s ++ "_buff")) "arr") pos) "just") (Dot expr "just")) ]
      print = printR ty (Ident "i") (LitString s) (Dot expr "just")
      new_dec  = [VarDecln Nothing (TypeSpec Int) "i" (Just (InitExpr (BinaryOp Sub (Ident "now") (LitInt lat))))]
      checkOk  = [If (BinaryOp Eq (Dot expr "nothing") (LitInt 0))
                     (save ++ print)]
      new_stmt = [For (Ident "i")
                      (BinaryOp LE (Ident "i") (Ident "now"))
                      (UnaryOp Inc (Ident "i"))
                      [IfElse (BinaryOp LOr (BinaryOp LT (Ident "i") (LitInt 0)) (Funcall (Ident ("ask" ++ show ty)) [Ident (s ++ "_buff"), Ident "i"]))
                              [Continue]
                              (estms ++ checkOk)]]
      decs = edecs ++ new_dec
      stms = new_stmt
  in (decs, stms)
        
-- Step function
step :: Specification -> FunDef
step spec = FunDef typ "step" params decln stms
  where
    typ    = TypeSpec Void
    params = [Param (Ptr (TypeSpec (TypedefName "input"))) "s"]
    decln  = []
    stms   = [Expr (Funcall (Ident "printf") [LitString "Instant %d:\n", Ident "now"])] ++
             map (\s -> Expr (Funcall (Ident ("eval_" ++ getIdent s)) (if (isInput s) then [Arrow (Ident "s") (getIdent s)] else []))) spec ++
             [Expr (Funcall (Ident "moveClock") []), Return Nothing]
 

-- | Finish funcion
             
finish :: Specification -> Int -> FunDef
finish spec maxLat = FunDef typ "finish" params decln stms
  where
    typ    = TypeSpec Void
    params = []
    decln  = [VarDecln Nothing (TypeSpec Int) "j" (Just (InitExpr (LitInt 0)))]
    stms   =    [ Expr (AssignOp Assign (Ident "inputEnd") (LitInt 1))
                , Expr (Funcall (Ident "undoClock") [])
                , Expr (Funcall (Ident "printf") [LitString "Finish:\n"])]
             ++ (if (null spec)
                 then []
                 else [For (Ident "j")
                        (BinaryOp LT (Ident "j") (LitInt (toInteger maxLat)))
                        (UnaryOp Inc (Ident "j"))
                        (map (\s -> Expr (Funcall (Ident ("eval_" ++ getIdent s)) [])) spec)])
             ++ [Return Nothing]
                        
                        
-- | Engine functions            
    
fundef :: Specification -> Integer -> TypeMap -> LatMap -> [FunDef]
fundef spec size typMap latMap = 
         [moveClock idents size, undoClock idents size] ++
         [initialise typMap size] ++
         map (\s -> evalGen s typMap latMap) spec ++
         [step spec] ++
         [finish specLat maxLat]
  where
    idents  = keys typMap
    maxLat  = foldr max 0 (map snd (toList latMap))
    specLat = filter (\s -> (getLat (getIdent s) latMap) > 0) spec

enginec :: Specification -> Integer -> TypeMap -> LatMap -> TransUnit
enginec spec size typMap latMap = TransUnit (declnc typMap size) (fundef spec size typMap latMap)
    

-- || Code generation

-- Compile to engine.h and engine.c files

compile :: String -> TransUnit -> TransUnit -> [String] -> IO ()
compile filename tuc tuh includes = do
  let cfile = render $ pretty $ translate $ tuc
      hfile = render $ pretty $ translate $ tuh
      cmacros = unlines [ "#include \"" ++ filename ++ ".h\""
                        , ""                        
                        ]
      hmacros = unlines ([ "#include \"data.h\""] ++
                         (map (\name -> "#include \"" ++ name ++ "\"") includes) ++
                         [""])
      hguards = unlines [ "#ifndef " ++ filename ++ "_H"
                        , "#define " ++ filename ++ "_H"
                        , ""]
      hguardsEnd = unlines ["", "#endif"]
                         
  writeFile ("monitor/" ++ filename ++ ".c") $ cmacros ++ cfile
  writeFile ("monitor/" ++ filename ++ ".h") $ hguards ++ hmacros ++ hfile ++ hguardsEnd

compileEngine :: Specification -> Integer -> TypeMap -> LatMap -> [String] -> IO ()
compileEngine spec size tyMap latMap includes = compile "engine" (enginec spec size tyMap latMap) engineh includes
