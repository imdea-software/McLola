{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts, TypeOperators #-}

module Theories.UAV where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.FreeVars
import Engine.Trans
import Internal.TypeRepr
import Internal.C99              (C99Fun, toC99Type, C99able(..))

import Data.Comp.Multi           ((:+:))
import Data.Comp.Multi.Derive    (derive, makeHFunctor, makeEqHF, smartConstructors)
import Language.C99.Simple       as LC99
import Control.Monad.State.Lazy  (get, put)

{-

  Intermediate Representation for a Data Theory to describe the state of the flight of an UAV:

* Types:
  
data Attitude = Attitude {yaw :: Double, roll :: Double, pitch :: Double}
data Target   = Target {x :: Double, y :: Double, mutex :: Double, num_wp :: Double}
data Position = Position {x :: Double, y :: Double, alt :: Double, zone :: Double}	
  
* Functions:
  
  attitude : Exp Double -> Exp Double -> Exp Double -> Exp Attitude
  yaw      : Exp Attitude -> Exp Double
  roll     : Exp Attitude -> Exp Double
  pitch    : Exp Attitude -> Exp Double
  
  target : Exp Double -> Exp Double -> Exp Double -> Exp Double -> Exp Target
  xt     : Exp Target -> Exp Double
  yt     : Exp Target -> Exp Double
  mutex  : Exp Target -> Exp Double
  num_wp : Exp Target -> Exp Double
  
  position : Exp Double -> Exp Double -> Exp Double -> Exp Double -> Exp Position
  xp       : Exp Position -> Exp Double
  yp       : Exp Position -> Exp Double
  alt      : Exp Position -> Exp Double
  zone     : Exp Position -> Exp Double
  
-}

data Attitude = Attitude {yaw :: Double, roll :: Double, pitch :: Double} deriving (Show, Eq)

data Target   = Target {xt :: Double, yt :: Double, mutex :: Double, num_wp :: Double} deriving (Show, Eq)

data Position = Position {xp :: Double, yp :: Double, alt :: Double, zone :: Double} deriving (Show, Eq)

-- | C99able instances

instance C99able Attitude where
  typeRepr = TNAProd "Attitude" [("yaw", TDouble), ("roll", TDouble), ("pitch", TDouble)]

instance C99able Target where
  typeRepr = TNAProd "Target" [("xt", TDouble), ("yt", TDouble), ("mutex", TDouble), ("num_wp", TDouble)]

instance C99able Position where
  typeRepr = TNAProd "Position" [("xp", TDouble), ("yp", TDouble), ("alt", TDouble), ("zone", TDouble)]

-- | Data Theory

-- Signature for data operators

data Att (e :: * -> *) a where
  Att   :: e Double -> e Double -> e Double -> Att e Attitude
  Yaw   :: e Attitude -> Att e Double
  Roll  :: e Attitude -> Att e Double
  Pitch :: e Attitude -> Att e Double

data Targ (e :: * -> *) a where
  Targ  :: e Double -> e Double -> e Double ->  e Double -> Targ e Target
  Xt    :: e Target -> Targ e Double
  Yt    :: e Target -> Targ e Double
  Mutex :: e Target -> Targ e Double
  NumWP :: e Target -> Targ e Double

data Pos (e :: * -> *) a where
  Pos  :: e Double -> e Double -> e Double ->  e Double -> Pos e Position
  Xp   :: e Position -> Pos e Double
  Yp   :: e Position -> Pos e Double
  Alt  :: e Position -> Pos e Double
  Zone :: e Position -> Pos e Double

type UAVTheory =  Att :+: Targ :+: Pos

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [''Att, ''Targ, ''Pos])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances

instance TempRefAlg Att where
  tempRefAlg (Att x y z) = TR $ unTR x ++ unTR y ++ unTR z
  tempRefAlg (Yaw x)     = TR $ unTR x
  tempRefAlg (Roll x)    = TR $ unTR x
  tempRefAlg (Pitch x)   = TR $ unTR x
  
instance TempRefAlg Targ where
  tempRefAlg (Targ x y w z) = TR $ unTR x ++ unTR y ++ unTR w ++ unTR z
  tempRefAlg (Xt x)         = TR $ unTR x
  tempRefAlg (Yt x)         = TR $ unTR x
  tempRefAlg (Mutex x)      = TR $ unTR x
  tempRefAlg (NumWP x)      = TR $ unTR x
  
instance TempRefAlg Pos where
  tempRefAlg (Pos x y w z) = TR $ unTR x ++ unTR y ++ unTR w ++ unTR z
  tempRefAlg (Xp x)        = TR $ unTR x
  tempRefAlg (Yp x)        = TR $ unTR x
  tempRefAlg (Alt x)       = TR $ unTR x
  tempRefAlg (Zone x)      = TR $ unTR x

-- | TypesAlg Instances

instance TypesAlg Att where
  typesAlg (Att x y z) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              (tsz, _) <- unTypes z
              return ([tyAtt] ++ tsx ++ tsy ++ tsz, tyAtt))
      where
        tyAtt = TNAProd "Attitude" [("yaw", TDouble), ("roll", TDouble), ("pitch", TDouble)]
  typesAlg (Yaw x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (Roll x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (Pitch x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))

instance TypesAlg Targ where
  typesAlg (Targ x y w z) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              (tsw, _) <- unTypes w
              (tsz, _) <- unTypes z
              return ([tyTarg] ++ tsx ++ tsy ++ tsw ++ tsz, tyTarg))
      where
        tyTarg = TNAProd "Target" [("xt", TDouble), ("yt", TDouble), ("mutex", TDouble), ("num_wp", TDouble)]
  typesAlg (Xt x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (Yt x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (Mutex x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (NumWP x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))

instance TypesAlg Pos where
  typesAlg (Pos x y w z) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              (tsw, _) <- unTypes w
              (tsz, _) <- unTypes z
              return ([tyPos] ++ tsx ++ tsy ++ tsw ++ tsz, tyPos))
      where
        tyPos = TNAProd "Position" [("xp", TDouble), ("yp", TDouble), ("alt", TDouble), ("zone", TDouble)]
  typesAlg (Xp x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (Yp x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (Alt x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (Zone x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))


-- | FreeVarsAlg Instances

instance FreeVarsAlg Att where
  freeVarsAlg (Att x y z) = FreeVars $ unFreeVars x ++ unFreeVars y ++ unFreeVars z
  freeVarsAlg (Yaw x)     = FreeVars $ unFreeVars x
  freeVarsAlg (Roll x)    = FreeVars $ unFreeVars x
  freeVarsAlg (Pitch x)   = FreeVars $ unFreeVars x
  
instance FreeVarsAlg Targ where
  freeVarsAlg (Targ x y w z) = FreeVars $ unFreeVars x ++ unFreeVars y ++ unFreeVars w ++ unFreeVars z
  freeVarsAlg (Xt x)         = FreeVars $ unFreeVars x
  freeVarsAlg (Yt x)         = FreeVars $ unFreeVars x
  freeVarsAlg (Mutex x)      = FreeVars $ unFreeVars x
  freeVarsAlg (NumWP x)      = FreeVars $ unFreeVars x
  
instance FreeVarsAlg Pos where
  freeVarsAlg (Pos x y w z) = FreeVars $ unFreeVars x ++ unFreeVars y ++ unFreeVars w ++ unFreeVars z
  freeVarsAlg (Xp x)        = FreeVars $ unFreeVars x
  freeVarsAlg (Yp x)        = FreeVars $ unFreeVars x
  freeVarsAlg (Alt x)       = FreeVars $ unFreeVars x
  freeVarsAlg (Zone x)      = FreeVars $ unFreeVars x

-- | C99DefAlg Instances

instance C99DefAlg Att where
  c99DefAlg (Att x y z) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             arg3 <- unC99 z
             (toMaybeFun c99Att) [arg1, arg2, arg3])
  c99DefAlg (Yaw x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Yaw) [arg1])
  c99DefAlg (Roll x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Roll) [arg1])
  c99DefAlg (Pitch x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Pitch) [arg1])
  
c99Att :: C99Fun
c99Att args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TNAProd "Attitude" [("yaw", TDouble), ("roll", TDouble), ("pitch", TDouble)]
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "yaw") (pr1 (args !! 0)))
                , Expr (AssignOp Assign (Dot expr "roll") (pr1 (args !! 1)))
                , Expr (AssignOp Assign (Dot expr "pitch") (pr1 (args !! 2)))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99Yaw :: C99Fun
c99Yaw args = return (Dot (pr1 (args !! 0)) "yaw", [], [], TDouble)

c99Roll :: C99Fun
c99Roll args = return (Dot (pr1 (args !! 0)) "roll", [], [], TDouble)
  
c99Pitch :: C99Fun
c99Pitch args = return (Dot (pr1 (args !! 0)) "pitch", [], [], TDouble)


instance C99DefAlg Targ where
  c99DefAlg (Targ x y w z) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             arg3 <- unC99 w
             arg4 <- unC99 z
             (toMaybeFun c99Targ) [arg1, arg2, arg3, arg4])
  c99DefAlg (Xt x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Xt) [arg1])
  c99DefAlg (Yt x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Yt) [arg1])
  c99DefAlg (Mutex x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Mutex) [arg1])
  c99DefAlg (NumWP x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99NumWP) [arg1])
  
c99Targ :: C99Fun
c99Targ args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TNAProd "Target" [("xt", TDouble), ("yt", TDouble), ("mutex", TDouble), ("num_wp", TDouble)]
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "xt") (pr1 (args !! 0)))
                , Expr (AssignOp Assign (Dot expr "yt") (pr1 (args !! 1)))
                , Expr (AssignOp Assign (Dot expr "mutex") (pr1 (args !! 2)))
                , Expr (AssignOp Assign (Dot expr "num_wp") (pr1 (args !! 3)))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)

c99Xt :: C99Fun
c99Xt args = return (Dot (pr1 (args !! 0)) "xt", [], [], TDouble)
  
c99Yt :: C99Fun
c99Yt args = return (Dot (pr1 (args !! 0)) "yt", [], [], TDouble)
  
c99Mutex :: C99Fun
c99Mutex args = return (Dot (pr1 (args !! 0)) "mutex", [], [], TDouble)

c99NumWP :: C99Fun
c99NumWP args = return (Dot (pr1 (args !! 0)) "num_wp", [], [], TDouble)

instance C99DefAlg Pos where
  c99DefAlg (Pos x y w z) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             arg3 <- unC99 w
             arg4 <- unC99 z
             (toMaybeFun c99Pos) [arg1, arg2, arg3, arg4])
  c99DefAlg (Xp x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Xp) [arg1])
  c99DefAlg (Yp x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Yp) [arg1])
  c99DefAlg (Alt x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Alt) [arg1])
  c99DefAlg (Zone x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Zone) [arg1])
  
c99Pos :: C99Fun
c99Pos args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty   = TNAProd "Position" [("xp", TDouble), ("yp", TDouble), ("alt", TDouble), ("zone", TDouble)]
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "xp") (pr1 (args !! 0)))
                , Expr (AssignOp Assign (Dot expr "yp") (pr1 (args !! 1)))
                , Expr (AssignOp Assign (Dot expr "alt") (pr1 (args !! 2)))
                , Expr (AssignOp Assign (Dot expr "zone") (pr1 (args !! 3)))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99Xp :: C99Fun
c99Xp args = return (Dot (pr1 (args !! 0)) "xp", [], [], TDouble)

c99Yp :: C99Fun
c99Yp args = return (Dot (pr1 (args !! 0)) "yp", [], [], TDouble)
  
c99Alt :: C99Fun
c99Alt args = return (Dot (pr1 (args !! 0)) "alt", [], [], TDouble)
  
c99Zone :: C99Fun
c99Zone args = return (Dot (pr1 (args !! 0)) "zone", [], [], TDouble)
