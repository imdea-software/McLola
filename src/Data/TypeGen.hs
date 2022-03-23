module Data.TypeGen (decType, fundefType) where
  
import Internal.TypeRepr
import Internal.C99         (toC99Type, format, maxList)

import Language.C99.Simple
import Prelude              hiding (LT)

{-

  Code generation for C99 specification's data types

-}

-- || Data types declaration

fieldsDec :: [(Ident, Type)] -> [FieldDecln]
fieldsDec = map fieldDec
  where
    fieldDec (name, ty) = FieldDecln ty name

typeDec :: TypeRepr -> [Decln]

typeDec ty@(TMaybe t) = [tyDec,
                         tyTypeDef]
  where
    tyDec     = TypeDecln (TypeSpec (StructDecln (Just (show ty)) (fieldsDec (zip ["just", "nothing"] (map toC99Type [t, TInt])))))
    tyTypeDef = VarDecln (Just Typedef) (TypeSpec (Struct (show ty))) (show ty) Nothing 

typeDec ty@(TEither t u) = [tyDec,
                            tyTypeDef]
  where
    tyDec     = TypeDecln (TypeSpec (StructDecln (Just (show ty)) (fieldsDec (zip ["left", "right", "opt"] (map toC99Type [t, u, TInt])))))
    tyTypeDef = VarDecln (Just Typedef) (TypeSpec (Struct (show ty))) (show ty) Nothing 
    
typeDec ty@(TList t) = [tyDec,
                        tyTypeDef]
  where
    tyDec     = TypeDecln (TypeSpec (StructDecln (Just (show ty)) (fieldsDec (zip ["data", "n", "size"] [Array (toC99Type t) (Just (LitInt (toInteger maxList))), TypeSpec Int, TypeSpec Int]))))
    tyTypeDef = VarDecln (Just Typedef) (TypeSpec (Struct (show ty))) (show ty) Nothing 
    
typeDec ty@(TProd t1 t2) = [tyDec,
                            tyTypeDef]
  where
    tyDec     = TypeDecln (TypeSpec (StructDecln (Just (show ty)) (fieldsDec (zip (map (\n -> "c" ++ (show n)) [1..]) (map toC99Type [t1, t2])))))
    tyTypeDef = VarDecln (Just Typedef) (TypeSpec (Struct (show ty))) (show ty) Nothing 
     
typeDec ty@(TNAProd name fs) = [tyDec,
                                tyTypeDef]
  where
    tyDec     = TypeDecln (TypeSpec (StructDecln (Just name) (fieldsDec (map (\(s, t) -> (s, toC99Type t)) fs))))
    tyTypeDef = VarDecln (Just Typedef) (TypeSpec (Struct name)) name Nothing
    
typeDec _ = []


funsDec :: TypeRepr -> [Decln]
funsDec ty = [eqDec, printDec]
  where
    eqDec       = FunDecln Nothing (TypeSpec Bool) ("equals" ++ show ty) [Param (toC99Type ty) "p1", Param (toC99Type ty) "p2"]
    printDec    = FunDecln Nothing (TypeSpec Void) ("print" ++ show ty) [Param (toC99Type ty) "p"]
    
decType :: TypeRepr -> [Decln]
decType ty = (typeDec ty) ++ (funsDec ty)


-- || Data types implementation

-- Equals function

equals :: TypeRepr -> FunDef
equals ty = FunDef typ ("equals" ++ show ty) params decln stms
  where
    typ    = TypeSpec Bool
    params = [Param (toC99Type ty) "p1", Param (toC99Type ty) "p2"]
    decln  = eqDecs ty
    stms   = eqStms ty

eqDecs :: TypeRepr -> [Decln]
eqDecs (TList t) = [ VarDecln Nothing (TypeSpec Int) "i" (Just (InitExpr (LitInt 0)))
                   , VarDecln Nothing (TypeSpec Bool) "r" (Just (InitExpr (LitBool True)))]
eqDecs _         = []

eqStms :: TypeRepr -> [Stmt]

eqStms t | builtin t = [Return (Just (BinaryOp Eq (Ident "p1") (Ident "p2")))]

eqStms (TMaybe t) = [Return (Just (BinaryOp LAnd eq1 eq2))]
  where
    eq1 = BinaryOp Eq (Dot (Ident "p1") "nothing") (Dot (Ident "p2") "nothing")
    eq2 = BinaryOp LOr (BinaryOp Eq (Dot (Ident "p1") "nothing") (LitInt 1))
                       (Funcall (Ident ("equals" ++ show t)) [Dot (Ident "p1") "just", Dot (Ident "p2") "just"])

eqStms (TEither t u) = [Return (Just (BinaryOp LAnd eq1 eq2))]
  where
    eq1 = BinaryOp Eq (Dot (Ident "p1") "opt") (Dot (Ident "p2") "opt")
    eq2 = Cond (BinaryOp Eq (Dot (Ident "p1") "opt") (LitInt 0))
               (Funcall (Ident ("equals" ++ show t)) [Dot (Ident "p1") "left", Dot (Ident "p2") "left"])
               (Funcall (Ident ("equals" ++ show u)) [Dot (Ident "p1") "right", Dot (Ident "p2") "right"])
                      
eqStms (TList t) = [ IfElse checkSize noteq checkData
                   , Return (Just (Ident "r"))]
  where
    checkSize = BinaryOp NEq (Dot (Ident "p1") "n") (Dot (Ident "p2") "n")
    noteq     = [Expr (AssignOp Assign (Ident "r") (LitBool False))]
    checkData = [For (Ident "i")
                     (BinaryOp LT (Ident "i") (Dot (Ident "p1") "n"))
                     (UnaryOp Inc (Ident "i"))
                     [If (UnaryOp Not (Funcall (Ident ("equals" ++ show t)) [Index (Dot (Ident "p1") "data") (Ident "i"), Index (Dot (Ident "p2") "data") (Ident "i")]))
                         [ Expr (AssignOp Assign (Ident "r") (LitBool False))
                         , Break]]]

eqStms (TProd t1 t2) = [Return (Just (BinaryOp LAnd eq1 eq2))]
  where
    eq1 = Funcall (Ident ("equals" ++ show t1)) [Dot (Ident "p1") "c1", Dot (Ident "p2") "c1"]
    eq2 = Funcall (Ident ("equals" ++ show t2)) [Dot (Ident "p1") "c2", Dot (Ident "p2") "c2"]

eqStms (TNAProd _ fs) = [Return (Just cmp)]
  where
    cmp  = foldr (\tp -> BinaryOp LAnd (eq tp)) (eq (last fs)) (init fs)
    eq (f, ty) = Funcall (Ident ("equals" ++ show ty)) [Dot (Ident "p1") f, Dot (Ident "p2") f]

-- Print function
               
prints :: TypeRepr -> FunDef
prints ty = FunDef typ ("print" ++ show ty) params decln stms
  where
    typ    = TypeSpec Void
    params = [Param (toC99Type ty) "p"]
    decln  = printDecs ty
    stms   = printStms ty

printDecs :: TypeRepr -> [Decln]

printDecs (TList t) = [VarDecln Nothing (TypeSpec Int) "i" (Just (InitExpr (LitInt 1)))]

printDecs _ = []


printStms :: TypeRepr -> [Stmt]

printStms TBool = [IfElse (Ident "p")
                    [Expr (Funcall (Ident "printf") [LitString (format TBool), LitString "true"])]
                    [Expr (Funcall (Ident "printf") [LitString (format TBool), LitString "false"])]]

printStms t | builtin t = [Expr (Funcall (Ident "printf") [LitString (format t), Ident "p"])]

printStms (TMaybe t) = [IfElse
                         (BinaryOp Eq (Dot (Ident "p") "nothing") (LitInt 1))
                         [Expr (Funcall (Ident "printf") [LitString "%s", LitString "nothing"])]
                         [ Expr (Funcall (Ident "printf") [LitString "%s", LitString "just ("])
                         , Expr (Funcall (Ident ("print" ++ show t)) [(Dot (Ident "p") "just")])
                         , Expr (Funcall (Ident "printf") [LitString "%s", LitString ")"])]
                       ]

printStms (TEither t u) = [IfElse
                            (BinaryOp Eq (Dot (Ident "p") "opt") (LitInt 0))
                            [ Expr (Funcall (Ident "printf") [LitString "%s", LitString "left ("])
                            , Expr (Funcall (Ident ("print" ++ show t)) [(Dot (Ident "p") "left")])
                            , Expr (Funcall (Ident "printf") [LitString "%s", LitString ")"])]
                            [ Expr (Funcall (Ident "printf") [LitString "%s", LitString "right ("])
                            , Expr (Funcall (Ident ("print" ++ show u)) [(Dot (Ident "p") "right")])
                            , Expr (Funcall (Ident "printf") [LitString "%s", LitString ")"])]
                          ]
                       
printStms (TProd t u) = [ Expr (Funcall (Ident "printf") [LitString "("])
                        , Expr (Funcall (Ident ("print" ++ show t)) [Dot (Ident "p") "c1"])
                        , Expr (Funcall (Ident "printf") [LitString ", "])
                        , Expr (Funcall (Ident ("print" ++ show u)) [Dot (Ident "p") "c2"])
                        , Expr (Funcall (Ident "printf") [LitString ")"])
                        ]
                       
printStms (TNAProd n fs) =    [Expr (Funcall (Ident "printf") [LitString "%s {", LitString n])]
                           ++ [ Expr (Funcall (Ident "printf") [LitString "%s = ", LitString (fst (head fs))])
                              , Expr (Funcall (Ident ("print" ++ show (snd (head fs)))) [Dot (Ident "p") (fst (head fs))])]
                           ++ (foldr (++) [] (
                                 map (\(field, ty) ->
                                            [ Expr (Funcall (Ident "printf") [LitString ", "])
                                            , Expr (Funcall (Ident "printf") [LitString "%s = ", LitString field])
                                            , Expr (Funcall (Ident ("print" ++ show ty)) [Dot (Ident "p") field])])
                                 (tail fs)))
                           ++ [Expr (Funcall (Ident "printf") [LitString "}"])]

printStms (TList TChar) = 
    [ Expr (Funcall (Ident "printf") [LitString "\""])
    , If (BinaryOp LT (LitInt 0) (Dot (Ident "p") "n"))
         [Expr (Funcall (Ident ("print" ++ show TChar)) [Index (Dot (Ident "p") "data") (LitInt 0)])]
    , For (Ident "i")
          (BinaryOp LT (Ident "i") (Dot (Ident "p") "n"))
          (UnaryOp Inc (Ident "i"))
          [ Expr (Funcall (Ident ("print" ++ show TChar)) [Index (Dot (Ident "p") "data") (Ident "i")])]
    , Expr (Funcall (Ident "printf") [LitString "\""])]

printStms (TList t) = 
    [ Expr (Funcall (Ident "printf") [LitString "["])
    , If (BinaryOp LT (LitInt 0) (Dot (Ident "p") "n"))
         [Expr (Funcall (Ident ("print" ++ show t)) [Index (Dot (Ident "p") "data") (LitInt 0)])]
    , For (Ident "i")
          (BinaryOp LT (Ident "i") (Dot (Ident "p") "n"))
          (UnaryOp Inc (Ident "i"))
          [ Expr (Funcall (Ident "printf") [LitString ", "])
          , Expr (Funcall (Ident ("print" ++ show t)) [Index (Dot (Ident "p") "data") (Ident "i")])]
    , Expr (Funcall (Ident "printf") [LitString "]"])]
         
                           
fundefType :: TypeRepr -> [FunDef]
fundefType ty = [equals ty, prints ty]
