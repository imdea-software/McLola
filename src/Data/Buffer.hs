module Data.Buffer (decBuffer, fundefBuffer) where

import Internal.TypeRepr         (TypeRepr(..))
import Internal.C99              (toC99Type, getDefault)

import Language.C99.Simple
import Prelude                   hiding (LT, GT, init)
import Control.Monad.State.Lazy  (evalState)
import Data.Map                  (empty)

{-

  Code generation for C99 buffer types

-}

-- || Buffer declarations

decBuffer :: TypeRepr -> [Decln]
decBuffer ty = [bufferDec,
                bufferTypeDef,
                initDec,
                putDec,
                getDec,
                askDec,
                offsetDec]
  where
    tyname        = "buffer" ++ show ty
    bufferDec     = TypeDecln (TypeSpec (StructDecln (Just tyname)
                        [FieldDecln (Ptr (toC99Type (TMaybe ty))) "arr",
                         FieldDecln (TypeSpec Int) "size"]))
    bufferTypeDef = VarDecln (Just Typedef) (TypeSpec (Struct tyname)) tyname Nothing
    initDec       = FunDecln Nothing (TypeSpec (TypedefName tyname)) ("init" ++ show ty) [Param (TypeSpec (TypedefName tyname)) "b", Param (Ptr (toC99Type (TMaybe ty))) "arr", Param (TypeSpec Int) "size"]
    putDec        = FunDecln Nothing (TypeSpec (TypedefName tyname)) ("put" ++ show ty) [Param (TypeSpec (TypedefName tyname)) "b", Param (toC99Type ty) "val", Param (TypeSpec Int) "i"]
    getDec        = FunDecln Nothing (toC99Type ty) ("get" ++ show ty) [Param (TypeSpec (TypedefName tyname)) "b", Param (TypeSpec Int) "i"]
    askDec        = FunDecln Nothing (TypeSpec Bool) ("ask" ++ show ty) [Param (TypeSpec (TypedefName tyname)) "b", Param (TypeSpec Int) "i"]
    offsetDec     = FunDecln Nothing (toC99Type (TMaybe ty)) ("offset" ++ show ty) [Param (TypeSpec (TypedefName tyname)) "b", Param (TypeSpec Int) "k", Param (toC99Type (TMaybe ty)) "def", Param (TypeSpec Int) "i", Param (TypeSpec Int) "now", Param (TypeSpec Int) "end"] 


-- || Buffer implementation

init :: TypeRepr -> FunDef
init ty = FunDef typ ("init" ++ show ty) params decln stms
  where
    typ    = (TypeSpec (TypedefName ("buffer" ++ show ty)))
    params = [Param typ "b", Param (Ptr (toC99Type (TMaybe ty))) "arr", Param (TypeSpec Int) "size"]
    
    (ed, dd, sd, _) = evalState (getDefault ty) (empty, "aux", 0, empty)
    
    decln  = dd ++ [VarDecln Nothing (TypeSpec Int) "i" (Just (InitExpr (LitInt 0)))]
    stms   = sd ++
             [Expr (AssignOp Assign (Dot (Ident "b") "arr") (Ident "arr")),
              For (Ident "i")
                    (BinaryOp LT (Ident "i") (Ident "size"))
                    (UnaryOp Inc (Ident "i"))
                     [Expr (AssignOp Assign (Dot (Index (Dot (Ident "b") "arr") (Ident "i")) "just") ed),
                      Expr (AssignOp Assign (Dot (Index (Dot (Ident "b") "arr") (Ident "i")) "nothing") (LitInt 1))],
              Expr (AssignOp Assign (Dot (Ident "b") "size") (Ident "size")),
              Return (Just (Ident "b"))]

put :: TypeRepr -> FunDef
put ty = FunDef typ ("put" ++ show ty) params decln stms
  where
    typ    = (TypeSpec (TypedefName ("buffer" ++ show ty)))
    params = [Param typ "b", Param (toC99Type ty) "val", Param (TypeSpec Int) "i"]
    decln  = []
    idx    = BinaryOp Mod (Ident "i") (Dot (Ident "b") "size")
    stms   = [Expr (AssignOp Assign (Dot (Index (Dot (Ident "b") "arr") idx) "just") (Ident "val")),
              Expr (AssignOp Assign (Dot (Index (Dot (Ident "b") "arr") idx) "nothing") (LitInt 0)),
              Return (Just (Ident "b"))]
           
get :: TypeRepr -> FunDef
get ty = FunDef typ ("get" ++ show ty) params decln stms
  where
    typ    = toC99Type ty
    typB   = (TypeSpec (TypedefName ("buffer" ++ show ty)))
    params = [Param typB "b", Param (TypeSpec Int) "i"]
    decln  = []
    idx    = BinaryOp Mod (Ident "i") (Dot (Ident "b") "size")
    stms   = [Return (Just (Dot (Index (Dot (Ident "b") "arr") idx) "just"))]
    
ask :: TypeRepr -> FunDef
ask ty = FunDef typ ("ask" ++ show ty) params decln stms
  where
    typ    = TypeSpec Bool
    typB   = (TypeSpec (TypedefName ("buffer" ++ show ty)))
    params = [Param typB "b", Param (TypeSpec Int) "i"]
    decln  = []
    idx    = BinaryOp Mod (Ident "i") (Dot (Ident "b") "size")
    rslt   = BinaryOp Eq (Dot (Index (Dot (Ident "b") "arr") idx) "nothing") (LitInt 0)
    stms   = [Return (Just rslt)]
                 
offset :: TypeRepr -> FunDef
offset ty = FunDef typ ("offset" ++ show ty) params decln stms
  where
    typ     = toC99Type (TMaybe ty)
    typB    = (TypeSpec (TypedefName ("buffer" ++ show ty)))
    params  = [Param typB "b", Param (TypeSpec Int) "k", Param (toC99Type (TMaybe ty)) "def", Param (TypeSpec Int) "i", Param (TypeSpec Int) "now", Param (TypeSpec Int) "end"]
    decln   = [VarDecln Nothing typ "off" Nothing]
    defStm  = [Expr (AssignOp Assign (Dot (Ident "off") "nothing") (Dot (Ident "def") "nothing")),
               Expr (AssignOp Assign (Dot (Ident "off") "just") (Dot (Ident "def") "just"))]
    idx     = BinaryOp Mod (BinaryOp Add (Ident "k") (Ident "i")) (Dot (Ident "b") "size")
    offPos  = Index (Dot (Ident "b") "arr") idx
    offStm  = [IfElse (BinaryOp GT (BinaryOp Add (Ident "k") (Ident "i")) (Ident "now"))
                      [Expr (AssignOp Assign (Dot (Ident "off") "nothing") (LitInt 1)),
                       Expr (AssignOp Assign (Dot (Ident "off") "just") (Dot offPos "just"))]
                      [Expr (AssignOp Assign (Dot (Ident "off") "nothing") (Dot offPos "nothing")),
                       Expr (AssignOp Assign (Dot (Ident "off") "just") (Dot offPos "just"))]]
    defCond = BinaryOp LOr
                (BinaryOp LT (BinaryOp Add (Ident "i") (Ident "k")) (LitInt 0))
                (BinaryOp And (BinaryOp GT (BinaryOp Add (Ident "i") (Ident "k")) (Ident "now")) (Ident "end"))
    stms    = [IfElse defCond
                      defStm 
                      offStm,
               Return (Just (Ident "off"))]
         
                       
fundefBuffer :: TypeRepr -> [FunDef]
fundefBuffer ty = [init ty, put ty, get ty, ask ty, offset ty]
