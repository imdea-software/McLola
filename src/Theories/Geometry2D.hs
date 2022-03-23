{-# Language GADTs, KindSignatures, TemplateHaskell, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TypeOperators, ConstraintKinds, FlexibleInstances #-}

module Theories.Geometry2D where

import StaticAnalysis.TempRef
import StaticAnalysis.Types
import Engine.Rewrite
import Engine.FreeVars
import Engine.Trans
import Internal.TypeRepr
import Internal.C99              (C99Fun, toC99Type, C99able(..))
import Theories.Bool             (BoolTheory, iIte, iAnd, iOr, iEq, iNot)
import Theories.Numeric
import Theories.Product          (ProdTheory, iFst, iSnd)
import Theories.LetBind          (LetBind, iLet, iRef)
import Theories.Either           (EitherTheory, iEitherL, iEitherR)
import Theories.List             (ListTheory, FoldableL, Foldable2L, iSinglL, iConsL, iTail, iHead, foldrL, concatL, zipL)

import Data.Comp.Multi           (Term, (:<:), project)
import Data.Comp.Multi.Derive    (derive, HFunctor, makeHFunctor, makeEqHF, smartConstructors)
import Language.C99.Simple       as LC99
import Control.Monad.State.Lazy  (get, put)

{-

  Intermediate Representation for 2-dimensional Geometry Data Theory.

* Types:

  data Point2 = P {x :: Double, y :: Double}
  
* Functions:
  
  point2D : Exp Double -> Exp Double -> Exp Point2
  x2D     : Exp Point2 -> Exp Double
  y2D     : Exp Point2 -> Exp Double
  
  plus       : Exp Point2 -> Exp Point2 -> Exp Point2
  minus      : Exp Point2 -> Exp Point2 -> Exp Point2
  dotprod    : Exp Point2 -> Exp Point2 -> Exp Double
  distance   : Exp Point2 -> Exp Point2 -> Exp Double
  norm       : Exp Point2 -> Exp Double
  cross      : Exp Point2 -> Exp Point2 -> Exp Double
  vectangle  : Exp Point2 -> Exp Point2 -> Exp Double
  scalarprod : Exp Double -> Exp Point2 -> Exp Point2
  cmpxdiv    : Exp Point2 -> Exp Point2 -> Exp Point2
  intersDist : Exp Point2 -> Exp Point2 -> Exp Point2 -> Exp Point2 -> Exp (Either String Double)
  pntSegDist : Exp Point2 -> Exp (Point2, Point2) -> Exp Double
  polySides  : Exp [Point2] -> Exp [(Point2, Point2)]
  pntInPoly  : Exp Point2 -> Exp [Point2] -> Exp Bool
  
-}

data Point2 = P {x :: Double, y :: Double} deriving (Show, Prelude.Eq)

-- | C99able instances

instance C99able Point2 where
  typeRepr = TNAProd "Point2" [("x", TDouble), ("y", TDouble)]
 
-- | Data Theory

-- Signature for data operators
data Point2D (e :: * -> *) a where
  Point2D :: e Double -> e Double -> Point2D e Point2
  X2D     :: e Point2 -> Point2D e Double
  Y2D     :: e Point2 -> Point2D e Double

type Geometry2DTheory = Point2D

-- Derive boilerplate code using Template Haskell
$(derive [makeHFunctor, makeEqHF, smartConstructors] 
         [''Point2D])


------------------------------------------------------------------------
-------------------------- Algebras Instances --------------------------
------------------------------------------------------------------------

-- | TempRefAlg Instances
  
instance TempRefAlg Point2D where
  tempRefAlg (Point2D x y) = TR $ unTR x ++ unTR y
  tempRefAlg (X2D x)       = TR $ unTR x
  tempRefAlg (Y2D x)       = TR $ unTR x

-- | TypesAlg Instances

instance TypesAlg Point2D where
  typesAlg (Point2D x y) =
    Types (do (tsx, _) <- unTypes x
              (tsy, _) <- unTypes y
              return ([tyPoint2] ++ tsx ++ tsy, tyPoint2))
    where tyPoint2 = TNAProd "Point2" [("x", TDouble), ("y", TDouble)]
  typesAlg (X2D x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))
  typesAlg (Y2D x) =
    Types (do (tsx, _) <- unTypes x
              return ([TDouble] ++ tsx, TDouble))

-- | FreeVarsAlg Instances

instance FreeVarsAlg Point2D where
  freeVarsAlg (Point2D x y) = FreeVars $ unFreeVars x ++ unFreeVars y
  freeVarsAlg (X2D x)       = FreeVars $ unFreeVars x
  freeVarsAlg (Y2D x)       = FreeVars $ unFreeVars x

-- | Rewrite Instances

instance (Point2D :<: v) => Rewrite Point2D v where
  rewriteAlg (X2D x) = case project x of
    Just (Point2D a b) -> a
    Nothing            -> iX2D x
  rewriteAlg (Y2D x) = case project x of
    Just (Point2D a b) -> b
    Nothing            -> iY2D x
  rewriteAlg (Point2D x y) = iPoint2D x y

-- | C99DefAlg Instances

instance C99DefAlg Point2D where
  c99DefAlg (Point2D x y) = 
    FC99 (do arg1 <- unC99 x
             arg2 <- unC99 y
             (toMaybeFun c99Point2D) [arg1, arg2])
  c99DefAlg (X2D x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99X2D) [arg1])
  c99DefAlg (Y2D x) = 
    FC99 (do arg1 <- unC99 x
             (toMaybeFun c99Y2D) [arg1])
  
c99Point2D :: C99Fun
c99Point2D args = 
  do (typeMap, str, n, bindMap) <- get
     let
         ty = TNAProd "Point2" [("x", TDouble), ("y", TDouble)]
         name = str ++ "_" ++ show n
         expr = Ident name
         decs = [VarDecln Nothing (toC99Type ty) name Nothing]
         stms = [ Expr (AssignOp Assign (Dot expr "x") (pr1 (args !! 0)))
                , Expr (AssignOp Assign (Dot expr "y") (pr1 (args !! 1)))]
     put (typeMap, str, n+1, bindMap)
     return (expr, decs, stms, ty)
  
c99X2D :: C99Fun
c99X2D args = return (Dot (pr1 (args !! 0)) "x", [], [], TDouble)
  
c99Y2D :: C99Fun
c99Y2D args = return (Dot (pr1 (args !! 0)) "y", [], [], TDouble)


-- | Derived operators

-- Plus

type PlusConstr e = (DBasicOp :<: e, Point2D :<: e)

iPlus :: PlusConstr e => Term e Point2 -> Term e Point2 -> Term e Point2
iPlus a b = iPoint2D xc yc
  where
    xc = iDAdd (iX2D a) (iX2D b)
    yc = iDAdd (iY2D a) (iY2D b)

-- Minus

type MinusConstr e = (DBasicOp :<: e, Point2D :<: e)

iMinus :: MinusConstr e => Term e Point2 -> Term e Point2 -> Term e Point2
iMinus a b = iPoint2D xc yc
  where
    xc = iDSub (iX2D a) (iX2D b)
    yc = iDSub (iY2D a) (iY2D b)
    
-- Dot Product

type DotPConstr e = (DBasicOp :<: e, Point2D :<: e)

iDotP :: DotPConstr e => Term e Point2 -> Term e Point2 -> Term e Double
iDotP a b = iDAdd px py
  where
    px = iDMul (iX2D a) (iX2D b)
    py = iDMul (iY2D a) (iY2D b)

-- Distance

type DistanceConstr e = (DBasicOp :<: e, Point2D :<: e)

iDistance :: DistanceConstr e => Term e Point2 -> Term e Point2 -> Term e Double
iDistance a b = iSqrt (iDAdd difx dify)
  where
    difx = iPow (iDSub (iX2D a) (iX2D b)) (iValDouble 2)
    dify = iPow (iDSub (iY2D a) (iY2D b)) (iValDouble 2)

-- Norm

type NormConstr e = (DistanceConstr e, Point2D :<: e)

iNorm :: NormConstr e => Term e Point2 -> Term e Double
iNorm p = iDistance p (iPoint2D (iValDouble 0) (iValDouble 0))
     
-- Cross Product

type CrossConstr e = (DBasicOp :<: e, Point2D :<: e)

iCross :: CrossConstr e => Term e Point2 -> Term e Point2 -> Term e Double
iCross a b = iDAdd (iDMul (iX2D a) (iY2D b)) (iDMul (iY2D a) (iX2D b))

-- Vectorial angle

type VectAngleConstr e = (CrossConstr e, Trig :<: e, DotPConstr e)

iVectAngle :: VectAngleConstr e => Term e Point2 -> Term e Point2 -> Term e Double
iVectAngle a b = iAtan2 (iCross a b) (iDotP a b)

-- Scalar Product

type ScalarMulConstr e = (Point2D :<: e, DBasicOp :<: e)

iScalarMul :: ScalarMulConstr e => Term e Double -> Term e Point2 -> Term e Point2
iScalarMul c a = iPoint2D (iDMul c (iX2D a)) (iDMul c (iY2D a))

-- Complex Division

type ComplexDivConstr e = (Point2D :<: e, LetBind :<: e, DBasicOp :<: e, HFunctor e, FreeVarsAlg e)

iComplexDiv :: ComplexDivConstr e => Term e Point2 -> Term e Point2 -> Term e Point2
iComplexDiv a b =
   let fs  = strsIn a ++ strsIn b
       [xa, ya, xb, yb, xc, yc] = fresh 6 fs
       xae = iRef xa
       yae = iRef ya
       xbe = iRef xb
       ybe = iRef yb
       n1  = iDAdd (iDMul xae xbe) (iDMul yae ybe)
       n2  = iDSub (iDMul yae xbe) (iDMul xae ybe)
       den = iDAdd (iDMul xbe xbe) (iDMul ybe ybe) 
   in
    iLet xa (iX2D a) (
    iLet ya (iY2D a) (
    iLet xb (iX2D b) (
    iLet yb (iY2D b) (
    iLet xc (iDDiv n1 den) (
    iLet yc (iDDiv n2 den) (
      iPoint2D (iRef xc) (iRef yc)
    ))))))

-- Intersection Distance

type IntersDistanceConstr e = (Point2D :<: e, DistanceConstr e, BoolTheory :<: e, DOrd :<: e, LetBind :<: e, DBasicOp :<: e, EitherTheory :<: e, ValChar :<: e, ListTheory :<: e, HFunctor e, FreeVarsAlg e) 

iIntersDistance :: IntersDistanceConstr e =>  Term e Point2 -> Term e Point2 -> Term e Point2 -> Term e Point2 -> Term e (Either String Double)
iIntersDistance a b c d =
   let fs = strsIn a ++ strsIn b ++ strsIn c ++ strsIn d
       [xv0, xv1, den, uavV, p0a, p0b, v0a, v0b, p1a, p1b, v1a, v1b, num, x1, x0, ip0, ip1, uavIP, targP, dist] = fresh 20 fs
      
       v0is0 = iEq (iRef xv0) (iValDouble 0)
       v1is0 = iEq (iRef xv1) (iValDouble 0)
       denIs0 = iEq (iRef den) (iValDouble 0)
       auvVLt0 = iDLt (iRef uavV) (iValDouble 0)
      
       msg1 = foldr (\ c -> iConsL (iValChar c)) (iSinglL (iValChar '.')) "Parallel vectors with x==0"
       err1 = iEitherL msg1 (iValDouble 1)
       msg2 = foldr (\ c -> iConsL (iValChar c)) (iSinglL (iValChar '.')) "Parallel vectors"
       err2 = iEitherL msg2 (iValDouble 1)
       msg3 = foldr (\ c -> iConsL (iValChar c)) (iSinglL (iValChar '.')) "Wrong direction"
       err3 = iEitherL msg3 (iValDouble 1)
       
       den_exp = iDSub (iDMul (iRef v0b) (iDDiv (iRef v1a) (iRef v0a))) (iRef v1b)
       num_exp = iDSub (iDSub (iRef p1b) (iRef p0b)) (iDMul (iDSub (iRef p1a) (iRef p0a)) (iDDiv (iRef v0b) (iRef v0a)))
       x1_exp = iDDiv (iRef num) (iRef den)
       x0_exp = iDDiv (iDAdd (iRef p1a) (iDSub (iDMul (iRef x1) (iRef v1a)) (iRef p0a))) (iRef v0a)
       ip0_exp = iPoint2D (iDAdd (iRef p0a) (iDMul (iRef x0) (iRef v0a))) (iDAdd (iRef p0b) (iDMul (iRef x0) (iRef v0b)))
       ip1_exp = iPoint2D (iDAdd (iRef p1a) (iDMul (iRef x1) (iRef v1a))) (iDAdd (iRef p1b) (iDMul (iRef x1) (iRef v1b)))
       uavIP_exp = iIte v0is0 (iRef ip1) (iRef ip0)
       targP_exp = iIte v0is0 (iPoint2D (iRef p0a) (iRef p0b)) (iPoint2D (iRef p1a) (iRef p1b))
       uavV_exp = iIte v0is0 (iRef x1) (iRef x0)
       dist_exp = iDistance (iRef uavIP) (iRef targP)
       
       compute =
        iLet p0a (iIte v0is0 (iX2D c) (iX2D a)) (
        iLet p0b (iIte v0is0 (iY2D c) (iY2D a)) (
        iLet v0a (iIte v0is0 (iX2D d) (iX2D b)) (
        iLet v0b (iIte v0is0 (iY2D d) (iY2D b)) (
        iLet p1a (iIte v0is0 (iX2D a) (iX2D c)) (
        iLet p1b (iIte v0is0 (iY2D a) (iY2D c)) (
        iLet v1a (iIte v0is0 (iX2D b) (iX2D d)) (
        iLet v1b (iIte v0is0 (iY2D b) (iY2D d)) (
        iLet den den_exp (
        iLet num num_exp (
        iLet x1 x1_exp (
        iLet x0 x0_exp (
        iLet ip0 ip0_exp (
        iLet ip1 ip1_exp (
        iLet uavIP uavIP_exp (
        iLet targP targP_exp (
        iLet uavV uavV_exp (
        iLet dist dist_exp (
          iIte denIs0
          err2
          (iIte auvVLt0
           err3
           (iEitherR msg1 (iRef dist)))
        ))))))))))))))))))
  
   in
     iLet xv0 (iX2D b) (
     iLet xv1 (iX2D d) (
      iIte (iAnd v0is0 v1is0)
      err1
      compute
     ))

-- Distance Point to Segment

type DistancePointSegConstr e = (Point2D :<: e, LetBind :<: e, ProdTheory :<: e, BoolTheory :<: e, MinusConstr e, ComplexDivConstr e, ScalarMulConstr e, DistanceConstr e, NormConstr e, DBasicOp :<: e, DOrd :<: e)
   
iDistancePointSeg :: DistancePointSegConstr e => Term e Point2 -> Term e (Point2, Point2) -> Term e Double
iDistancePointSeg p s =
    let fs = strsIn p ++ strsIn s
        [a, b, bmina, q, qx, qy] = fresh 6 fs
        ae     = iRef a
        be     = iRef b
        bminae = iRef bmina
        qe     = iRef q
        qxe    = iRef qx
        qye    = iRef qy
        
        cond   = iAnd (iDLeq (iValDouble 0) qxe) (iDLeq qxe (iValDouble 1))
        exp1   = iNorm (iScalarMul qye bminae)
        exp2   = iDMin (iDistance p ae) (iDistance p be)
    
    in  iLet a (iFst s) (
        iLet b (iSnd s) (
        iLet bmina (iMinus be ae) (
        iLet q (iComplexDiv (iMinus p ae) bminae) (
        iLet qx (iX2D qe) (
        iLet qy (iY2D qe) (
           iIte cond exp1 exp2
        ))))))

  
-- Polygon Sides

type PolygonSidesConstr e = (Foldable2L e, FoldableL e, ProdTheory :<: e)

polygonSides :: PolygonSidesConstr e => Term e [Point2] -> Term e [(Point2, Point2)]
polygonSides poly = zipL poly (concatL (iTail poly) (iSinglL (iHead poly)))


-- Point in Poly

type PointInPolyConstr e = (FoldableL e, PolygonSidesConstr e, Parity :<: e, IBasicOp :<: e, DBasicOp :<: e, BoolTheory :<: e, DOrd :<: e, ProdTheory :<: e, Point2D :<: e)

pointInPoly :: PointInPolyConstr e => Term e Point2 -> Term e [Point2] -> Term e Bool

pointInPoly p poly = iOdd (foldrL fun (iValInt 0) (polygonSides poly))
 where
  fun = \ side n ->
   let 
     -- Fresh names generation for let bindings
     fs  = strsIn p ++ strsIn poly
     [pxnm, pynm, axnm, aynm, bxnm, bynm, mnm, bnm] = fresh 8 fs
     
     -- boolean conditions
     
     found = (iEq n (iValInt (-1)))
     
     far = iNot (between py ay by)
     
     onSegment = iIte (iEq ay by) 
                   (between px ax bx)
                   online
     
     rayIntersects = iAnd intersects
                     (iAnd (iOr (iNot (iEq py ay)) (iDLt by py))
                           (iOr (iNot (iEq py by)) (iDLt ay py)))
        
     -- auxiliar constructions
     
     between x a b = iIte (iDGt a b) 
                       (iAnd (iDLeq b x) (iDLeq x a))
                       (iAnd (iDLeq a x) (iDLeq x b))
     
     online = iIte (iEq ax bx)
                (iEq px ax)
                (iEq py (iDAdd (iDMul m px) b))
     
     intersects = iIte (iEq ax bx)
                    (iDLeq px ax)
                    (iIte (iDLt m (iValDouble 0))
                      (iDLeq py (iDAdd (iDMul m px) b))
                      (iDGeq py (iDAdd (iDMul m px) b)))
              
     pxe = iX2D p
     pye = iY2D p
     axe = iX2D (iFst side)
     aye = iY2D (iFst side)
     bxe = iX2D (iSnd side)
     bye = iY2D (iSnd side)
     me = iDDiv (iDSub ay by) (iDSub ax bx)
     be = iDSub ay (iDMul m ax)
     
     px = iRef pxnm
     py = iRef pynm
     ax = iRef axnm
     ay = iRef aynm
     bx = iRef bxnm
     by = iRef bynm
     m = iRef mnm
     b = iRef bnm
        
   in
     -- Point in Poly computation
     
     iLet pxnm pxe (
     iLet pynm pye (
     iLet axnm axe (
     iLet aynm aye (
     iLet bxnm bxe (
     iLet bynm bye (
     iLet mnm me (
     iLet bnm be (
       iIte found
         n
         (iIte far
           n
           (iIte onSegment
             (iValInt (-1))
             (iIte rayIntersects
               (iIAdd n (iValInt 1))
               n)))
     ))))))))
