{-# Language GADTs, KindSignatures, TemplateHaskell, TypeOperators #-}

module Theories.Simpl where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.FreeVars
import Engine.Trans
import Internal.C99              (C99Fun, toC99Type)

import Data.Comp.Multi           ((:+:))
import Data.Comp.Multi.Derive    (derive, makeHFunctor, makeEqHF)
import Language.C99.Simple       as LC99
import Control.Monad.State.Lazy  (get, put)

{-

  Intermediate Representation for abstractions to implement simplifiers.
  (Internal Data Theory)

* Types:

  -
  
* Operators:
  
  choice : Exp a -> Exp a -> Exp a
  iteK   : Exp a -> Exp b -> Exp b -> Exp b
  
-}

-- | Signatures for abstractions to implement simplifiers

data Choice e a where
  Choice :: e a -> e a -> Choice e a

data IteK e a where
  IteK :: e a -> e b -> e b -> IteK e b

type Abstr = Choice :+: IteK

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF] 
         [''Choice, ''IteK])

-- | Commutative operator
comm :: C99SimAlg e => (t -> t -> e FC99 i) -> t -> t -> Choice FC99 i
comm f e1 e2 = Choice (c99SimAlg (f e1 e2)) (c99SimAlg (f e2 e1))


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances

instance TempRefAlg Choice where
  tempRefAlg (Choice x y) = TR $ unTR x ++ unTR y
  
instance TempRefAlg IteK where
  tempRefAlg (IteK b x y) = TR $ unTR b ++ unTR x ++ unTR y

-- | TypesAlg Instances

instance TypesAlg Choice where
  typesAlg (Choice x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))

instance TypesAlg IteK where
  typesAlg (IteK b x y) =
    Types (do (tsb, _) <- unTypes b
              (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsb ++ tsx ++ tsy, t))

-- | FreeVarsAlg Instances

instance FreeVarsAlg Choice where
  freeVarsAlg (Choice x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  
instance FreeVarsAlg IteK where
  freeVarsAlg (IteK b x y) = FreeVars $ unFreeVars b ++ unFreeVars x ++ unFreeVars y

-- | C99DefAlg Instances

instance C99DefAlg Choice where
  c99DefAlg (Choice x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             c99choice [arg1, arg2])

c99choice :: C99Fun
c99choice args = 
  do 
     (typeMap, str, n, bindMap) <- get 
     let
         name = str ++ "_" ++ show n
         expr = Ident name
         ty   = pr4 (args !! 0)
         decs = foldr (++) [] (map pr2 args) ++ [VarDecln Nothing (toC99Type ty) name Nothing]
        
         just n  = BinaryOp LC99.Eq (Dot (pr1 (args !! n)) "nothing") (LitInt 0)
         known n = [ Expr (AssignOp Assign (Dot expr "nothing") (LitInt 0))
                   , Expr (AssignOp Assign (Dot expr "just") (Dot (pr1 (args !! n)) "just"))]
         unknown = [ Expr (AssignOp Assign (Dot expr "nothing") (LitInt 1))
                   , Expr (AssignOp Assign (Dot expr "just") (Dot (pr1 (args !! 1)) "just"))]
         
         stmt = [IfElse (just 0)
                   (known 0)
                   [IfElse (just 1)
                      (known 1)
                      unknown]]
         stms = foldr (++) [] (map pr3 args) ++ stmt
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
instance C99DefAlg IteK where
  c99DefAlg (IteK b x y) =
    FC99 (do arg1 <- unC99 b
             arg2 <- unC99 x
             arg3 <- unC99 y
             c99IteK [arg1, arg2, arg3])

c99IteK :: C99Fun
c99IteK args =
  do (typeMap, str, n, bindMap) <- get
     let
         name = str ++ "_" ++ show n
         expr = Ident name
         ty   = pr4 (args!!1)
         decs = foldr (++) [] (map pr2 args) ++ [VarDecln Nothing (toC99Type ty) name Nothing]
         
         known = BinaryOp LC99.Eq (Dot (pr1 (args !! 0)) "nothing") (LitInt 0)
         arg n = [ Expr (AssignOp Assign (Dot expr "nothing") (Dot (pr1 (args !! n)) "nothing"))
                 , Expr (AssignOp Assign (Dot expr "just") (Dot (pr1 (args !! n)) "just"))]
         
         stmt = [IfElse known
                   (arg 1)
                   (arg 2)]
         stms = foldr (++) [] (map pr3 args) ++ stmt
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
