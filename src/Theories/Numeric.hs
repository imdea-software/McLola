{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts, TypeOperators #-}

module Theories.Numeric where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.FreeVars
import Engine.Trans
import Internal.TypeRepr
import Internal.C99              (C99, C99Fun, toC99Type, C99able(..))
import Theories.Simpl            (comm)
import Theories.Bool             (BoolTheory(Eq, Ite))

import Data.Comp.Multi           ((:+:))
import Data.Comp.Multi.Derive    (derive, makeHFunctor, makeEqHF, smartConstructors)
import Language.C99.Simple       as LC99
import Control.Monad.State.Lazy  (get, put)

{-

  Intermediate Representation for Numeric Data Theory

* Types:

  Int
  Double
  Char (only constants)
  
* Functions:
  
  Common numerical functions for Int and Double
  lit : a -> Exp a
  +   : Exp a -> Exp a -> Exp a
  -   : Exp a -> Exp a -> Exp a
  *   : Exp a -> Exp a -> Exp a
  /   : Exp a -> Exp a -> Exp a
  abs : Exp a -> Exp a
  
  Extra for Double functions
  sqrt :: Exp Double -> Exp Double
  pow  :: Exp Double -> Exp Double -> Exp Double
  
  Common order predicates for Int and Double 
  <   : Exp a -> Exp a -> Exp Bool
  <=  : Exp a -> Exp a -> Exp Bool
  >   : Exp a -> Exp a -> Exp Bool
  >=  : Exp a -> Exp a -> Exp Bool
  min : Exp a -> Exp a -> Exp a
  max : Exp a -> Exp a -> Exp a
  
  Parity functions for Int
  odd  : Exp Int -> Exp Bool
  even : Exp Int -> Exp Bool
  
  Trigonometry functions
  atan2 :: Double -> Double -> Double
  
  Conversion functions
  intToDouble : Exp Int -> Exp Double
  doubleToInt : Exp Double -> Exp Int
  
  Char constants
  lit : Char -> Exp Char
  
-}

-- | C99able instances

instance C99able Int where
  typeRepr = TInt

instance C99able Double where
  typeRepr = TDouble

instance C99able Char where
  typeRepr = TChar

-- | Signature for data operators

-- Integer operators

data IBasicOp e a where
  ValInt :: Int -> IBasicOp e Int
  IAdd   :: e Int -> e Int -> IBasicOp e Int
  ISub   :: e Int -> e Int -> IBasicOp e Int
  IMul   :: e Int -> e Int -> IBasicOp e Int
  IDiv   :: e Int -> e Int -> IBasicOp e Int
  IAbs   :: e Int -> IBasicOp e Int

data IOrd e a where
  ILt  :: e Int -> e Int -> IOrd e Bool
  ILeq :: e Int -> e Int -> IOrd e Bool
  IGt  :: e Int -> e Int -> IOrd e Bool
  IGeq :: e Int -> e Int -> IOrd e Bool
  IMin :: e Int -> e Int -> IOrd e Int
  IMax :: e Int -> e Int -> IOrd e Int
  
data Parity e a where
  Odd  :: e Int -> Parity e Bool
  Even :: e Int -> Parity e Bool

-- Double operators

data DBasicOp e a where
  ValDouble :: Double -> DBasicOp e Double
  DAdd      :: e Double -> e Double -> DBasicOp e Double
  DSub      :: e Double -> e Double -> DBasicOp e Double
  DMul      :: e Double -> e Double -> DBasicOp e Double
  DDiv      :: e Double -> e Double -> DBasicOp e Double
  DAbs      :: e Double -> DBasicOp e Double
  Sqrt      :: e Double -> DBasicOp e Double
  Pow       :: e Double -> e Double -> DBasicOp e Double

data DOrd e a where
  DLt  :: e Double -> e Double -> DOrd e Bool
  DLeq :: e Double -> e Double -> DOrd e Bool
  DGt  :: e Double -> e Double -> DOrd e Bool
  DGeq :: e Double -> e Double -> DOrd e Bool
  DMin :: e Double -> e Double -> DOrd e Double
  DMax :: e Double -> e Double -> DOrd e Double

data Trig e a where
  Atan2 :: e Double -> e Double -> Trig e Double

-- Conversion operators

data Conv e a where
  IntToDouble :: e Int -> Conv e Double
  DoubleToInt :: e Double -> Conv e Int

-- Char constants

data ValChar (e :: * -> *) a where
  ValChar :: Char -> ValChar e Char

-- | Data Theory

type IntTheory     = IBasicOp :+: IOrd :+: Parity
type DoubleTheory  = DBasicOp :+: DOrd :+: Trig
type NumericTheory = IntTheory :+: DoubleTheory :+: Conv :+: ValChar

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [ ''IBasicOp, ''IOrd, ''Parity
         , ''DBasicOp, ''DOrd, ''Trig
         , ''Conv
         , ''ValChar ])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances

instance TempRefAlg IBasicOp where
  tempRefAlg (ValInt _) = TR []
  tempRefAlg (IAdd x y) = TR $ unTR x ++ unTR y
  tempRefAlg (ISub x y) = TR $ unTR x ++ unTR y
  tempRefAlg (IMul x y) = TR $ unTR x ++ unTR y
  tempRefAlg (IDiv x y) = TR $ unTR x ++ unTR y
  tempRefAlg (IAbs x)   = TR $ unTR x
  
instance TempRefAlg IOrd where
  tempRefAlg (ILt x y)  = TR $ unTR x ++ unTR y
  tempRefAlg (ILeq x y) = TR $ unTR x ++ unTR y
  tempRefAlg (IGt x y)  = TR $ unTR x ++ unTR y
  tempRefAlg (IGeq x y) = TR $ unTR x ++ unTR y
  tempRefAlg (IMin x y) = TR $ unTR x ++ unTR y
  tempRefAlg (IMax x y) = TR $ unTR x ++ unTR y
  
instance TempRefAlg Parity where
  tempRefAlg (Odd x)  = TR $ unTR x
  tempRefAlg (Even x) = TR $ unTR x
  
instance TempRefAlg DBasicOp where
  tempRefAlg (ValDouble _) = TR []
  tempRefAlg (DAdd x y) = TR $ unTR x ++ unTR y
  tempRefAlg (DSub x y) = TR $ unTR x ++ unTR y
  tempRefAlg (DMul x y) = TR $ unTR x ++ unTR y
  tempRefAlg (DDiv x y) = TR $ unTR x ++ unTR y
  tempRefAlg (DAbs x)   = TR $ unTR x
  tempRefAlg (Sqrt x)   = TR $ unTR x
  tempRefAlg (Pow x y)  = TR $ unTR x ++ unTR y
  
instance TempRefAlg DOrd where
  tempRefAlg (DLt x y)  = TR $ unTR x ++ unTR y
  tempRefAlg (DLeq x y) = TR $ unTR x ++ unTR y
  tempRefAlg (DGt x y)  = TR $ unTR x ++ unTR y
  tempRefAlg (DGeq x y) = TR $ unTR x ++ unTR y
  tempRefAlg (DMin x y) = TR $ unTR x ++ unTR y
  tempRefAlg (DMax x y) = TR $ unTR x ++ unTR y
  
instance TempRefAlg Conv where
  tempRefAlg (IntToDouble x) = TR $ unTR x
  tempRefAlg (DoubleToInt x) = TR $ unTR x
  
instance TempRefAlg Trig where
  tempRefAlg (Atan2 x y) = TR $ unTR x ++ unTR y

instance TempRefAlg ValChar where
  tempRefAlg (ValChar _) = TR []

-- | TypesAlg Instances
              
instance TypesAlg IBasicOp where
  typesAlg (ValInt _) = Types $ return ([TInt], TInt)
  typesAlg (IAdd x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, t2) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (ISub x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (IMul x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, t)) -- TBool in case of simplifier
  typesAlg (IDiv x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (IAbs x) =
    Types (do (tsx, t) <- unTypes x
              return (tsx, t))
 
instance TypesAlg IOrd where
  typesAlg (ILt x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (ILeq x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (IGt x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (IGeq x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (IMin x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (IMax x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))

instance TypesAlg Parity where
  typesAlg (Odd x) =
    Types (do (tsx, _) <- unTypes x
              return (TBool:tsx, TBool))
  typesAlg (Even x) =
    Types (do (tsx, _) <- unTypes x
              return (TBool:tsx, TBool))

instance TypesAlg DBasicOp where
  typesAlg (ValDouble _) = Types $ return ([TDouble], TDouble)
  typesAlg (DAdd x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (DSub x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (DMul x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, t)) -- TBool in case of simplifier
  typesAlg (DDiv x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (DAbs x) =
    Types (do (tsx, t) <- unTypes x
              return (tsx, t))
  typesAlg (Sqrt x) =
    Types (do (tsx, t) <- unTypes x
              return (tsx, t))
  typesAlg (Pow x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
 
instance TypesAlg DOrd where
  typesAlg (DLt x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (DLeq x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (DGt x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (DGeq x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([TBool] ++ tsx ++ tsy, TBool))
  typesAlg (DMin x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
  typesAlg (DMax x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))
              
instance TypesAlg Conv where
  typesAlg (IntToDouble x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (DoubleToInt x) =
    Types (do (tsx, _) <- unTypes x
              return ([TInt] ++ tsx, TInt))

instance TypesAlg Trig where
  typesAlg (Atan2 x y) =
    Types (do (tsx, t) <- unTypes x
              (tsy, _) <- unTypes y
              return (tsx ++ tsy, t))

instance TypesAlg ValChar where
  typesAlg (ValChar _) = Types $ return ([TChar], TChar)
  
-- | FreeVarsAlg Instances
  
instance FreeVarsAlg IBasicOp where
  freeVarsAlg (ValInt _) = FreeVars []
  freeVarsAlg (IAdd x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (ISub x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (IMul x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (IDiv x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (IAbs x)   = FreeVars $ unFreeVars x
  
instance FreeVarsAlg IOrd where
  freeVarsAlg (ILt x y)  = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (ILeq x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (IGt x y)  = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (IGeq x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (IMin x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (IMax x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  
instance FreeVarsAlg Parity where
  freeVarsAlg (Odd x)  = FreeVars $ unFreeVars x
  freeVarsAlg (Even x) = FreeVars $ unFreeVars x
  
instance FreeVarsAlg DBasicOp where
  freeVarsAlg (ValDouble _) = FreeVars []
  freeVarsAlg (DAdd x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DSub x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DMul x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DDiv x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DAbs x)   = FreeVars $ unFreeVars x
  freeVarsAlg (Sqrt x)   = FreeVars $ unFreeVars x
  freeVarsAlg (Pow x y)  = FreeVars $ unFreeVars x ++ unFreeVars y
  
instance FreeVarsAlg DOrd where
  freeVarsAlg (DLt x y)  = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DLeq x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DGt x y)  = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DGeq x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DMin x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (DMax x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  
instance FreeVarsAlg Conv where
  freeVarsAlg (IntToDouble x) = FreeVars $ unFreeVars x
  freeVarsAlg (DoubleToInt x) = FreeVars $ unFreeVars x

instance FreeVarsAlg Trig where
  freeVarsAlg (Atan2 x y) = FreeVars $ unFreeVars x ++ unFreeVars y

instance FreeVarsAlg ValChar where
  freeVarsAlg (ValChar _) = FreeVars []

-- | C99DefAlg Instances

instance C99DefAlg IBasicOp where
  c99DefAlg (ValInt d) = FC99 $ (toMaybeVal . c99ValInt) d 
  c99DefAlg (IAdd x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Add) [arg1, arg2])
  c99DefAlg (ISub x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Sub) [arg1, arg2])
  c99DefAlg (IMul x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Mul) [arg1, arg2])
  c99DefAlg (IDiv x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Div) [arg1, arg2])
  c99DefAlg (IAbs x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Abs) [arg1])

c99ValInt :: Int -> C99
c99ValInt d = return (LitInt (toInteger d), [], [], TInt)

c99Add :: C99Fun
c99Add args = return (BinaryOp Add (pr1 (args !! 0)) (pr1 (args !! 1)), [], [], pr4 (args !! 0))

c99Sub :: C99Fun
c99Sub args = return (BinaryOp Sub (pr1 (args !! 0)) (pr1 (args !! 1)), [], [], pr4 (args !! 0))

c99Mul :: C99Fun
c99Mul args = return (BinaryOp Mult (pr1 (args !! 0)) (pr1 (args !! 1)), [], [], pr4 (args !! 0))

c99Div :: C99Fun
c99Div args = return (BinaryOp Div (pr1 (args !! 0)) (pr1 (args !! 1)), [], [], pr4 (args !! 0))

c99Abs :: C99Fun
c99Abs args = case t0 of
                TInt    -> return (Cond (BinaryOp LC99.LT a0 (LitInt 0)) (BinaryOp Mult (LitInt (-1)) a0) a0, [], [], t0)
                TDouble -> return (Cond (BinaryOp LC99.LT a0 (LitDouble 0)) (BinaryOp Mult (LitDouble (-1)) a0) a0, [], [], t0)
  where a0 = pr1 (args !! 0)
        t0 = pr4 (args !! 0)

instance C99DefAlg IOrd where
  c99DefAlg (ILt x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Lt) [arg1, arg2])
  c99DefAlg (ILeq x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Leq) [arg1, arg2])
  c99DefAlg (IGt x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Gt) [arg1, arg2])
  c99DefAlg (IGeq x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Geq) [arg1, arg2])
  c99DefAlg (IMin x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Min) [arg1, arg2])
  c99DefAlg (IMax x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Max) [arg1, arg2])
             
c99Lt :: C99Fun
c99Lt args = return (BinaryOp LC99.LT (pr1 (args !! 0)) (pr1 (args !! 1)), [], [], TBool)

c99Leq :: C99Fun
c99Leq args = return (BinaryOp LE (pr1 (args !! 0)) (pr1 (args !! 1)), [], [], TBool)

c99Gt :: C99Fun
c99Gt args = return (BinaryOp LC99.GT (pr1 (args !! 0)) (pr1 (args !! 1)), [], [], TBool)

c99Geq :: C99Fun
c99Geq args = return (BinaryOp GE (pr1 (args !! 0)) (pr1 (args !! 1)), [], [], TBool)

c99Min :: C99Fun
c99Min args = return (Cond (BinaryOp LC99.LT a0 a1) a0 a1, [], [], t0)
  where (a0, _, _, t0) = args !! 0
        a1 = pr1 (args !! 1)
        
c99Max :: C99Fun
c99Max args = return (Cond (BinaryOp LC99.LT a0 a1) a1 a0, [], [], t0)
  where (a0, _, _, t0) = args !! 0
        a1 = pr1 (args !! 1)

instance C99DefAlg Parity where
  c99DefAlg (Odd x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Odd) [arg1])
  c99DefAlg (Even x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Even) [arg1])

c99Odd :: C99Fun
c99Odd args = return (Cond (BinaryOp LC99.Eq (BinaryOp Mod a0 (LitInt 2)) (LitInt 0)) (LitBool False) (LitBool True), [], [], TBool)
  where a0 = pr1 (args !! 0)

c99Even :: C99Fun
c99Even args = return (Cond (BinaryOp LC99.Eq (BinaryOp Mod a0 (LitInt 2)) (LitInt 0)) (LitBool True) (LitBool False), [], [], TBool)
  where a0 = pr1 (args !! 0)
  
instance C99DefAlg DBasicOp where
  c99DefAlg (ValDouble d) = FC99 $ (toMaybeVal . c99ValDouble) d 
  c99DefAlg (DAdd x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Add) [arg1, arg2])
  c99DefAlg (DSub x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Sub) [arg1, arg2])
  c99DefAlg (DMul x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Mul) [arg1, arg2])
  c99DefAlg (DDiv x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Div) [arg1, arg2])
  c99DefAlg (DAbs x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Abs) [arg1])
  c99DefAlg (Sqrt x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Sqrt) [arg1])
  c99DefAlg (Pow x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Pow) [arg1, arg2])
  
c99ValDouble :: Double -> C99
c99ValDouble d = return (LitDouble d, [], [], TDouble)

c99Sqrt :: C99Fun
c99Sqrt args = return (Funcall (Ident "sqrt") [pr1 (args !! 0)], [], [], pr4 (args !! 0))

c99Pow :: C99Fun
c99Pow args = return (Funcall (Ident "pow") [pr1 (args !! 0), pr1 (args !! 1)], [], [], pr4 (args !! 0))

instance C99DefAlg DOrd where
  c99DefAlg (DLt x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Lt) [arg1, arg2])
  c99DefAlg (DLeq x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Leq) [arg1, arg2])
  c99DefAlg (DGt x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Gt) [arg1, arg2])
  c99DefAlg (DGeq x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Geq) [arg1, arg2])
  c99DefAlg (DMin x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Min) [arg1, arg2])
  c99DefAlg (DMax x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Max) [arg1, arg2])

instance C99DefAlg Conv where
  c99DefAlg (IntToDouble x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99IntToDouble) [arg1])
  c99DefAlg (DoubleToInt x) =
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99DoubleToInt) [arg1])

c99IntToDouble :: C99Fun
c99IntToDouble args = return (Funcall (Ident "toDouble") [pr1 (args !! 0)], [], [], pr4 (args !! 0))

c99DoubleToInt :: C99Fun
c99DoubleToInt args = return (Funcall (Ident "toInt") [pr1 (args !! 0)], [], [], pr4 (args !! 0))

instance C99DefAlg Trig where
  c99DefAlg (Atan2 x y) =
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Atan2) [arg1, arg2])

c99Atan2 :: C99Fun
c99Atan2 args = return (Funcall (Ident "atan2") [pr1 (args !! 0), pr1 (args !! 1)], [], [], pr4 (args !! 0))

instance C99DefAlg ValChar where
  c99DefAlg (ValChar c) = FC99 $ (toMaybeVal . c99ValChar) c
  
c99ValChar :: Char -> C99
c99ValChar c = return (LitInt ((toInteger . fromEnum) c), [], [], TChar)

-- | C99SimAlg Instances

instance C99SimAlg IBasicOp where
  c99SimAlg (IMul x y) = c99SimAlg (comm mulOp x y)
    where
      mulOp x y = Ite
             (c99DefAlg (Theories.Bool.Eq x (c99DefAlg (ValInt 0)))) 
             x 
             (c99DefAlg (IMul x y))
  c99SimAlg e = c99DefAlg e
  
instance C99SimAlg DBasicOp where
  c99SimAlg (DMul x y) = c99SimAlg (comm mulOp x y)
    where
      mulOp x y = Ite
             (c99DefAlg (Theories.Bool.Eq x (c99DefAlg (ValDouble 0)))) 
             x 
             (c99DefAlg (DMul x y))
  c99SimAlg e = c99DefAlg e
