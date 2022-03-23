{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators, ConstraintKinds #-}

module Theories.List where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.FreeVars
import Engine.Trans
import Internal.Maps
import Internal.TypeRepr
import Internal.C99              (C99Fun, toC99Type, maxList, C99able(..))
import Theories.LetBind          (LetBind, iLet, iRef)
import Theories.Bool             (BoolTheory, iValBool, iEq, iOr, iIte)
import Theories.Numeric          (IBasicOp, IOrd, iISub, iValInt, iIMin)
import Theories.Product          (ProdTheory, iPair)

import Data.Comp.Multi           (Term, (:<:))
import Data.Comp.Multi.Derive    (HFunctor, derive, makeHFunctor, makeEqHF, smartConstructors)
import Language.C99.Simple       as LC99 hiding (Init)
import Data.Maybe                (fromMaybe)
import Control.Monad.State.Lazy  (get, put)

{-

  Intermediate Representation for List Data Theory.

* Types:

  [t]
  
* Functions:
  
  empty     : Exp a -> Exp [a]
  singleton : Exp a -> Exp [a]
  cons      : Exp a -> Exp [a] -> Exp [a]
  
  length : Exp [a] -> Exp Int
  index  : Exp [a] -> Exp Int -> Exp a
  head   : Exp [a] -> Exp a
  tail   : Exp [a] -> Exp [a]
  init   : Exp [a] -> Exp [a]
  last   : Exp [a] -> Exp a
  foldr  : (Exp a -> Exp b -> Exp b) -> Exp b -> Exp [a] -> Exp b
  foldr2 : (Exp a -> Exp b -> Exp c -> Exp c) -> Exp c -> Exp [a] -> Exp [b] -> Exp c
  concat : Exp [a] -> Exp [a] -> Exp [a]
  elem   : Exp a -> Exp [a] -> Exp Bool
  zip    : Exp [a] -> Exp [b] -> Exp [(a, b)]

-}

-- | C99able instance

instance C99able a => C99able [a] where
  typeRepr = TList (typeRepr @a)

-- | Data Theory

data ListTheory (e :: * -> *) (a :: *) where
  EmptyL      :: e a -> ListTheory e [a] -- default value
  SinglL      :: e a -> ListTheory e [a]
  ConsL       :: e a -> e [a] -> ListTheory e [a]
  Length      :: e [a] -> ListTheory e Int
  Indx        :: e [a] -> e Int -> ListTheory e a 
  Head        :: e [a] -> ListTheory e a
  Tail        :: e [a] -> ListTheory e [a]
  Init        :: e [a] -> ListTheory e [a]
  Last        :: e [a] -> ListTheory e a
  -- Internal operators to implement higher order functions
  WhileFoldr  :: String -> String -> String -> e a -> e [b] -> ListTheory e a
  WhileFoldr2 :: String -> String -> String -> String -> e a -> e [b] -> e [c] -> ListTheory e a

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [''ListTheory])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances

instance TempRefAlg ListTheory where
  tempRefAlg (EmptyL x)  = TR $ unTR x
  tempRefAlg (SinglL x)  = TR $ unTR x
  tempRefAlg (ConsL x y) = TR $ unTR x ++ unTR y
  tempRefAlg (Length x)  = TR $ unTR x
  tempRefAlg (Indx x y)  = TR $ unTR x ++ unTR y
  tempRefAlg (Head x)    = TR $ unTR x
  tempRefAlg (Tail x)    = TR $ unTR x
  tempRefAlg (Init x)    = TR $ unTR x
  tempRefAlg (Last x)    = TR $ unTR x
  tempRefAlg (WhileFoldr _ _ _ x y)      = TR $ unTR x ++ unTR y
  tempRefAlg (WhileFoldr2 _ _ _ _ x y z) = TR $ unTR x ++ unTR y ++ unTR z

-- | TypesAlg Instances

instance TypesAlg ListTheory where
  typesAlg (EmptyL x) =
    Types (do (tsx, t) <- unTypes x
              return ([TList t] ++ tsx, TList t))
  typesAlg (SinglL x) =
    Types (do (tsx, t) <- unTypes x
              return ([TList t] ++ tsx, TList t))
  typesAlg (ConsL x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, t) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (Length x) =
    Types (do (tsx, _) <- unTypes x
              return ([TInt] ++ tsx, TInt))
  typesAlg (Indx x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              case t of
                TList ty -> return ([ty] ++ tsx ++ tsy, ty)
                _        -> error "type error")
  typesAlg (Head x) =
    Types (do (tsx, t) <- unTypes x
              case t of
                TList ty -> return ([ty] ++ tsx, ty)
                _        -> error "type error")
  typesAlg (Tail x) =
    Types (do (tsx, t) <- unTypes x
              return (tsx, t))
  typesAlg (Init x) =
    Types (do (tsx, t) <- unTypes x
              return (tsx, t))
  typesAlg (Last x) =
    Types (do (tsx, t) <- unTypes x
              case t of
                TList ty -> return ([ty] ++ tsx, ty)
                _        -> error "type error")
  typesAlg (WhileFoldr _ _ _ x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (WhileFoldr2 _ _ _ _ x y z) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              (tsz, _) <- unTypes z
              return (tsx ++ tsy ++ tsz, t))

-- | FreeVarsAlg Instances

instance FreeVarsAlg ListTheory where
  freeVarsAlg (EmptyL x)  = FreeVars $ unFreeVars x
  freeVarsAlg (SinglL x)  = FreeVars $ unFreeVars x
  freeVarsAlg (ConsL x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (Length x)  = FreeVars $ unFreeVars x
  freeVarsAlg (Indx x y)  = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (Head x)    = FreeVars $ unFreeVars x
  freeVarsAlg (Tail x)    = FreeVars $ unFreeVars x
  freeVarsAlg (Init x)    = FreeVars $ unFreeVars x
  freeVarsAlg (Last x)    = FreeVars $ unFreeVars x
  freeVarsAlg (WhileFoldr _ _ _ x y)      = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (WhileFoldr2 _ _ _ _ x y z) = FreeVars $ unFreeVars x ++ unFreeVars y ++ unFreeVars z

-- | C99DefAlg Instances

instance C99DefAlg ListTheory where
  c99DefAlg (EmptyL x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99EmptyL) [arg1])
  c99DefAlg (SinglL x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99SinglL) [arg1])
  c99DefAlg (ConsL x y) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99ConsL) [arg1, arg2])
  c99DefAlg (Length x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Length) [arg1])
  c99DefAlg (Indx x y) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Indx) [arg1, arg2])
  c99DefAlg (Head x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Head) [arg1])
  c99DefAlg (Tail x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Tail) [arg1])
  c99DefAlg (Last x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Last) [arg1])
  c99DefAlg (Init x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Init) [arg1])
  c99DefAlg (WhileFoldr i acc aa x y) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun (c99WhileFoldr i acc aa x)) [arg1, arg2])
  c99DefAlg (WhileFoldr2 i acc a b x y z) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             arg3 <- unC99 z
             (toMaybeFun (c99WhileFoldr2 i acc a b x)) [arg1, arg2, arg3])
  
c99EmptyL :: C99Fun
c99EmptyL args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TList (pr4 (args !! 0))
         name = str ++ "_" ++ show n
         expr = Ident name
         idx  = str ++ "_i_" ++ show n
         i    = Ident idx
         decs = [ VarDecln Nothing (toC99Type ty) name Nothing
                , VarDecln Nothing (toC99Type TInt) idx Nothing ]
         stms = [ Expr (AssignOp Assign (Dot expr "n") (LitInt 0))
                , Expr (AssignOp Assign (Dot expr "size") (LitInt maxList)) 
                , For (AssignOp Assign i (LitInt 0)) (BinaryOp LC99.LT i (Dot expr "size")) (UnaryOp Inc i)
                    [Expr (AssignOp Assign (Index (Dot expr "data") i) (pr1 (args !! 0)))] ] -- default for misraC
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99SinglL :: C99Fun
c99SinglL args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TList (pr4 (args !! 0))
         name = str ++ "_" ++ show n
         expr = Ident name
         idx  = str ++ "_i_" ++ show n
         i    = Ident idx
         decs = [ VarDecln Nothing (toC99Type ty) name Nothing
                , VarDecln Nothing (toC99Type TInt) idx Nothing ]
         stms = [ Expr (AssignOp Assign (Dot expr "n") (LitInt 1))
                , Expr (AssignOp Assign (Dot expr "size") (LitInt maxList)) 
                , For (AssignOp Assign i (LitInt 0)) (BinaryOp LC99.LT i (Dot expr "size")) (UnaryOp Inc i)
                    [Expr (AssignOp Assign (Index (Dot expr "data") i) (pr1 (args !! 0)))] ] -- default for misraC
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)

c99ConsL :: C99Fun
c99ConsL args = 
  do (typeMap, str, n, bindMap) <- get
     let
         a0   = args !! 0
         a1   = args !! 1
         ty   = pr4 a1
         name = str ++ "_" ++ show n
         expr = Ident name
         idx  = str ++ "_i_" ++ show (n+1)
         i    = Ident idx
         new_n = Cond (BinaryOp LC99.LT (Dot (pr1 a1) "n") (LitInt maxList)) (BinaryOp Add (Dot (pr1 a1) "n") (LitInt 1)) (Dot (pr1 a1) "n")
         decs = [ VarDecln Nothing (toC99Type ty) name Nothing
                , VarDecln Nothing (toC99Type TInt) idx Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "n") new_n)
                , Expr (AssignOp Assign (Dot expr "size") (LitInt maxList))
                , Expr (AssignOp Assign (Index (Dot expr "data") (LitInt 0)) (pr1 a0))
                , For (AssignOp Assign i (LitInt 1)) (BinaryOp LC99.LT i (LitInt maxList)) (UnaryOp Inc i)
                    [Expr (AssignOp Assign (Index (Dot expr "data") i) (Index (Dot (pr1 a1) "data") (BinaryOp LC99.Sub i (LitInt 1))))] ]-- default for misraC
     put (typeMap, str, n+2, bindMap)
     return (expr, decs, stms, ty)

c99Length :: C99Fun
c99Length args = 
  return (Dot (pr1 (args !! 0)) "n", [], [], TInt)
  
c99Indx :: C99Fun
c99Indx args = 
  do (typeMap, str, n, bindMap) <- get
     let
         a0   = args !! 0
         a1   = args !! 1
         ty   = case (pr4 a0) of
                  TList t -> t
                  _       -> error "err"
         expr = Index (Dot (pr1 a0) "data") (pr1 a1)
     return (expr, [], [], ty)
  
c99Head :: C99Fun
c99Head args = 
  do (typeMap, str, n, bindMap) <- get
     let
         a0   = args !! 0
         ty   = case (pr4 a0) of
                  TList tt -> tt
                  _        -> error "err"
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [ VarDecln Nothing (toC99Type ty) name Nothing ]
         stms = [ Expr (AssignOp Assign expr (Index (Dot (pr1 a0) "data") (LitInt 0))) ]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99Tail :: C99Fun
c99Tail args = 
  do (typeMap, str, n, bindMap) <- get
     let
         a0   = args !! 0
         ty   = pr4 a0
         name = str ++ "_" ++ show n
         expr = Ident name
         idx  = str ++ "_i_" ++ show (n+1)
         i    = Ident idx
         decs = [ VarDecln Nothing (toC99Type ty) name Nothing 
                , VarDecln Nothing (toC99Type TInt) idx Nothing]
         new_n = Cond (BinaryOp LC99.Eq (Dot (pr1 a0) "n") (LitInt 0)) (LitInt 0) (BinaryOp LC99.Sub (Dot (pr1 a0) "n") (LitInt 1))
         stms = [ Expr (AssignOp Assign (Dot expr "n") new_n)
                , Expr (AssignOp Assign (Dot expr "size") (LitInt maxList))
                , For (AssignOp Assign i (LitInt 0)) (BinaryOp LC99.LT i (Dot expr "n")) (UnaryOp Inc i)
                    [Expr (AssignOp Assign (Index (Dot expr "data") i) (Index (Dot (pr1 a0) "data") (BinaryOp LC99.Add i (LitInt 1))))] ]
     put (typeMap, str, n+2, bindMap)
     return (expr, decs, stms, ty)
  
c99Last :: C99Fun
c99Last args = 
  do (typeMap, str, n, bindMap) <- get
     let
         a0   = args !! 0
         ty   = case (pr4 a0) of
                  TList tt -> tt
                  _        -> error "err"
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [ VarDecln Nothing (toC99Type ty) name Nothing ]
         stms = [ Expr (AssignOp Assign expr (Index (Dot (pr1 a0) "data") (BinaryOp LC99.Sub (Dot (pr1 a0) "n") (LitInt 1)))) ]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99Init :: C99Fun
c99Init args = 
  do (typeMap, str, n, bindMap) <- get
     let
         a0   = args !! 0
         ty   = pr4 a0
         name = str ++ "_" ++ show n
         expr = Ident name
         idx  = str ++ "_i_" ++ show (n+1)
         i    = Ident idx
         decs = [ VarDecln Nothing (toC99Type ty) name Nothing 
                , VarDecln Nothing (toC99Type TInt) idx Nothing]
         new_n = Cond (BinaryOp LC99.Eq (Dot (pr1 a0) "n") (LitInt 0)) (LitInt 0) (BinaryOp LC99.Sub (Dot (pr1 a0) "n") (LitInt 1))
         stms = [ Expr (AssignOp Assign (Dot expr "n") new_n)
                , Expr (AssignOp Assign (Dot expr "size") (LitInt maxList))
                , For (AssignOp Assign i (LitInt 0)) (BinaryOp LC99.LT i (Dot expr "n")) (UnaryOp Inc i)
                    [Expr (AssignOp Assign (Index (Dot expr "data") i) (Index (Dot (pr1 a0) "data") i))] ]
     put (typeMap, str, n+2, bindMap)
     return (expr, decs, stms, ty)

c99WhileFoldr :: String -> String -> String -> FC99 e -> C99Fun
c99WhileFoldr i acc aa exp args = 
  do (typeMap, str, n, bindMap) <- get
     (ea, da, sa, ta) <- unC99 exp
     let
         idx      = str ++ "_i_" ++ show n
         i_name   = snd $ fromMaybe (error "err") (lookupS i bindMap)
         acc_name = snd $ fromMaybe (error "err") (lookupS acc bindMap)
         aa_name  = snd $ fromMaybe (error "err") (lookupS aa bindMap)
         ty = pr4 (args !! 0)
         expr = Dot (Ident acc_name) "just" 
         decs = (VarDecln Nothing (toC99Type TInt) idx Nothing) : da
         stms = [ Expr (AssignOp Assign (Ident idx) (Dot (Ident i_name) "just"))
                , For (Ident idx) (BinaryOp GE (Ident idx) (LitInt 0)) (UnaryOp Dec ((Ident idx)))
                  (  [ Expr (AssignOp Assign ((Dot (Ident aa_name) "just")) (Index (Dot (pr1 (args !! 1)) "data") (Ident idx)))  ]
                  ++ sa
                  ++ [ Expr (AssignOp Assign (Ident acc_name) ea) ] ) ]
     return (expr, decs, stms, ty)
  
c99WhileFoldr2 :: String -> String -> String -> String -> FC99 e -> C99Fun
c99WhileFoldr2 i acc a b exp args = 
  do (typeMap, str, n, bindMap) <- get
     (ea, da, sa, ta) <- unC99 exp
     let
         idx      = str ++ "_i_" ++ show n
         i_name   = snd $ fromMaybe (error "err") (lookupS i bindMap)
         acc_name = snd $ fromMaybe (error "err") (lookupS acc bindMap)
         a_name   = snd $ fromMaybe (error "err") (lookupS a bindMap)
         b_name   = snd $ fromMaybe (error "err") (lookupS b bindMap)
         ty = pr4 (args !! 0)
         expr = Dot (Ident acc_name) "just" 
         decs = (VarDecln Nothing (toC99Type TInt) idx Nothing) : da
         stms = [ Expr (AssignOp Assign (Ident idx) (Dot (Ident i_name) "just"))
                , For (Ident idx) (BinaryOp GE (Ident idx) (LitInt 0)) (UnaryOp Dec ((Ident idx)))
                  (  [ Expr (AssignOp Assign ((Dot (Ident a_name) "just")) (Index (Dot (pr1 (args !! 1)) "data") (Ident idx))) 
                     , Expr (AssignOp Assign ((Dot (Ident b_name) "just")) (Index (Dot (pr1 (args !! 2)) "data") (Ident idx))) ]
                  ++ sa
                  ++ [ Expr (AssignOp Assign (Ident acc_name) ea) ] ) ]
     return (expr, decs, stms, ty)


-- | Derived operators:

-- Foldl

type FoldableL e = (HFunctor e, FreeVarsAlg e, ListTheory :<: e, IBasicOp :<: e, LetBind :<: e)

foldrL :: FoldableL e => (Term e a -> Term e b -> Term e b) -> Term e b -> Term e [a] -> Term e b
foldrL f e xs = 
  let
     fs0 = strsIn e ++ strsIn xs
     i   = fresh1 fs0
     fs  = fs0 ++ (strsIn (f (iIndx xs (iRef i)) e)) ++ [i]
     [acc,a] = fresh 2 fs
  in iLet acc e (
     iLet i (iISub (iLength xs) (iValInt 1)) (
     iLet a (iIndx xs (iRef i)) (
       iWhileFoldr i acc a (f (iRef a) (iRef acc)) xs
     )))

-- Foldl2

type Foldable2L e = (HFunctor e, FreeVarsAlg e, ListTheory :<: e, IBasicOp :<: e, IOrd :<: e, LetBind :<: e)

foldr2L :: Foldable2L e => (Term e a -> Term e b -> Term e c -> Term e c) -> Term e c -> Term e [a] -> Term e [b] -> Term e c
foldr2L f e xs ys = 
  let
     fs0 = strsIn e ++ strsIn xs ++ strsIn ys
     i   = fresh1 fs0
     fs  = fs0 ++ (strsIn (f (iIndx xs (iRef i)) (iIndx ys (iRef i)) e)) ++ [i]
     [acc,a,b] = fresh 3 fs
  in iLet acc e (
     iLet i (iIMin (iISub (iLength xs) (iValInt 1)) (iISub (iLength ys) (iValInt 1))) (
     iLet a (iIndx xs (iRef i)) (
     iLet b (iIndx ys (iRef i)) (
       iWhileFoldr2 i acc a b (f (iRef a) (iRef b) (iRef acc)) xs ys
     ))))

-- Concat

type ConcatConstr e = FoldableL e

concatL :: ConcatConstr e => Term e [a] -> Term e [a] -> Term e [a]
concatL xs ys = foldrL iConsL ys xs

-- Elem

type ElemConstr e = (FoldableL e, BoolTheory :<: e)

elemL :: ElemConstr e => Term e a -> Term e [a] -> Term e Bool
elemL x xs = foldrL fun (iValBool False) xs
  where
    fun = \ a p -> iOr p (iEq a x)

-- Zip

type ZipConstr e = (Foldable2L e, ProdTheory :<: e)

zipL :: ZipConstr e => Term e [a] -> Term e [b] -> Term e [(a, b)]
zipL xs ys = iInit $ foldr2L (\ a b c -> iConsL (iPair a b) c) (iSinglL (iPair (iHead xs) (iHead ys))) xs ys


-- Filter elems

type FilterElemConstr e = (FoldableL e, ElemConstr e, BoolTheory :<: e)

filterElem :: FilterElemConstr e => Term e a -> Term e [a] -> Term e [a] -> Term e [a]
filterElem def elems xs = foldrL (\e res -> iIte (elemL e elems) (iConsL e res) res) (iEmptyL def) xs
