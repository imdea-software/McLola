{-# Language GADTs, TemplateHaskell, TypeOperators, FlexibleContexts, ConstraintKinds, KindSignatures #-}

module Language.Lola where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.FreeVars
import Engine.Trans
import Internal.TypeRepr
import Internal.Maps             (getType)
import Internal.C99              (C99, toC99Type, C99able)
import Theories.Theories

import Data.Comp.Multi           (Term, EqHF(..), (:+:))
import Data.Comp.Multi.Derive    (derive, makeHFunctor, smartConstructors, keq)
import Language.C99.Simple
import Control.Monad.State.Lazy  (get, put)
import Data.Typeable             (Typeable)

{-

  MCLola Streams and Expressions

-}

-- || Identifiers and Streamable

type Identifier   = String
type Streamable a = (Typeable a, C99able a)


-- || Streams

data Stream a where
  In  :: Streamable a => Identifier          -> Stream a
  Out :: Streamable a => (Identifier, Exp a) -> Stream a
  
instance Show (Stream a) where
  show (In s)      = s
  show (Out (s,_)) = s


-- || Expressions

type Exp a = Term Sig a

-- | Signature for Lola expressions

type Sig = Temp :+: Data :+: Abstr :+: LetBind
          
-- | Data theories

type Data =     BoolTheory :+: NumericTheory
            :+: ProdTheory :+: MaybeTheory :+: EitherTheory :+: ListTheory
            :+: Geometry2DTheory :+: UAVTheory

-- | Temporal operators

data Temp (e :: * -> *) a where
  Now    :: Streamable a => Stream a -> Temp e a
  Offset :: Streamable a => Stream a -> Int -> e a -> Temp e a

-- Instances of equality for higher-order signatures Now and Offset
instance EqHF Temp where
  eqHF (Now (In s)) (Now (In t))           = s == t
  eqHF (Now (Out (s,_))) (Now (Out (t,_))) = s == t -- unique names, expressions need not be checked
  eqHF (Offset (In s) n x) (Offset (In t) m y)           = (s == t) && (n == m) && keq x y
  eqHF (Offset (Out (s,_)) n x) (Offset (Out (t,_)) m y) = (s == t) && (n == m) && keq x y
  eqHF _ _                                               = False

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, smartConstructors]
         [''Temp])
         
------------------------------------------------------------------------
--------------- Algebras Instances for Temporal Operators --------------
------------------------------------------------------------------------

ident :: Stream a -> Identifier
ident = show

-- || TempRefAlg Instances

instance TempRefAlg Temp where
  tempRefAlg (Now s)        = TR $ [(ident s, 0)]
  tempRefAlg (Offset s k x) = TR $ [(ident s, k)] ++ unTR x
  
-- || TypesAlg Instances

instance TypesAlg Temp where
  typesAlg (Now s) =
    Types (do (tm, _) <- get
              let t = getType (ident s) tm
              return ([t], t))
  typesAlg (Offset s k x) = x

-- || FreeVarsAlg Instances

instance FreeVarsAlg Temp where
  freeVarsAlg (Now s)        = FreeVars []
  freeVarsAlg (Offset s k x) = x

-- || C99DefAlg Instances

instance C99DefAlg Temp where
  c99DefAlg (Now s)        = FC99 (c99Now s)
  c99DefAlg (Offset s k x) = FC99 (c99Offset s k x)

c99Now :: Stream a -> C99
c99Now s =
  do (typeMap, str, n, bindMap) <- get
     let
         name  = str ++ "_" ++ show n
         expr  = Ident name
         ty    = getType (ident s) typeMap
         decs  = [VarDecln Nothing (toC99Type (TMaybe ty)) name Nothing]
         getE  = Funcall (Ident ("get" ++ show ty))
                         [Ident ((ident s) ++ "_buff"), Ident "i"]
         known = Cast (TypeName (TypeSpec Bool)) -- cast for Misra C compliance
                      (Funcall (Ident ("ask" ++ show ty))
                         [Ident ((ident s) ++ "_buff"), Ident "i"])
         stms  = [ IfElse known
                   [Expr (AssignOp Assign (Dot expr "nothing") (LitInt 0))]
                   [Expr (AssignOp Assign (Dot expr "nothing") (LitInt 1))]
                 , Expr (AssignOp Assign (Dot expr "just") getE)]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, TMaybe ty)

c99Offset :: Stream a -> Int -> FC99 e -> C99
c99Offset s k x =
  do (ed, dd, sd, _) <- unC99 x
     (typeMap, str, n, bindMap) <- get
     let
         name = str ++ "_" ++ show n
         expr = Ident name
         ty   = getType (ident s) typeMap
         decs = dd ++ [VarDecln Nothing (toC99Type (TMaybe ty)) name Nothing]
         offE = Funcall (Ident ("offset" ++ show ty))
                        [Ident ((ident s) ++ "_buff"), LitInt (toInteger k), ed, Ident "i", Ident "now", Ident "inputEnd"]
         stms = sd ++ [Expr (AssignOp Assign expr offE)]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, TMaybe ty)

