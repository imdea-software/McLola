module Examples.UAV.UAV1 (spec) where

import MCLola
import Theories.Bool
import Theories.Numeric
import Theories.Geometry2D
import Theories.UAV
import Theories.Maybe
import Theories.List

-- | Input streams

attitude :: Stream Attitude
attitude = In "attitude"

velocity :: Stream Point2
velocity = In "velocity"

position :: Stream Position
position = In "position"

altitude :: Stream Double
altitude = In "altitude"

target :: Stream Target
target = In "target"

nofly :: Stream [Point2]
nofly = In "nofly"

events_within :: Stream [Int]
events_within = In "events_within"
    
-- | Output streams

-- Auxiliar streams

filtered_pos :: (Exp Position -> Exp Double) -> String -> Stream Double
filtered_pos field name  = Out (name, expr)
  where
    cond = iILt (iNow instantN) (iValInt 3)
    nowcomp = field (iNow position)
    this = filtered_pos field name
    calc = iDDiv (iDAdd nowcomp (iDMul (iValDouble 0.6) (iDSub (iDMul (iValDouble 2) (iOffset this (-1) (iValDouble 0))) (iDDiv (iOffset this (-2) (iValDouble 0)) (iValDouble 2))))) (iValDouble 1.9)
    expr  = iIte cond nowcomp calc

filtered_pos_xp :: Stream Double
filtered_pos_xp = filtered_pos iXp "filtered_pos_xp"
   
filtered_pos_yp :: Stream Double
filtered_pos_yp = filtered_pos iYp "filtered_pos_yp"
  
filtered_pos_alt :: Stream Double
filtered_pos_alt = filtered_pos iAlt "filtered_pos_alt"

instantN :: Stream Int
instantN = Out ("instantN", expr)
  where
    expr = iIAdd (iValInt 1) (iOffset instantN (-1) (iValInt 0))

-- Output stream: all_ok_capture

roll_ok :: Stream Bool
roll_ok = Out ("roll_ok", expr)
  where
    eRoll = iDAbs (iRoll (iNow attitude))
    expr  = iDLt eRoll (iValDouble 0.0523)

pitch_ok :: Stream Bool
pitch_ok = Out ("pitch_ok", expr)
  where
    ePitch = iDAbs (iPitch (iNow attitude))
    expr   = iDLt ePitch (iValDouble 0.0523)

height_ok :: Stream Bool
height_ok = Out ("height_ok", expr)
  where
    expr  = iDGt (iNow filtered_pos_alt) (iValDouble 0)

near :: Stream Bool
near = Out ("near", expr)
  where
    filPos = iPoint2D (iNow filtered_pos_xp) (iNow filtered_pos_yp)
    tarPos = iPoint2D (iXt (iNow target)) (iYt (iNow target))
    e_dist = iDistance filPos tarPos
    expr   = iDLt e_dist (iValDouble 1)
    
open_capture :: Stream Bool
open_capture = Out ("open_capture", expr)
  where
    rel        = foldr (\cod -> iConsL (iValInt cod)) (iSinglL (iValInt 45)) [50, 51] -- 45 -> "capture", 50 -> "yes_person", 51 -> "no_person"
    relEvents  = filterElem (iValInt 0) rel (iNow events_within)
    def        = iOffset open_capture (-1) (iValBool False)
    lastIsCapt = iEq (iLast relEvents) (iValInt 45) -- 45 -> "capture"
    expr       = iIte (iEq (iValInt 0) (iLength relEvents)) def lastIsCapt
    
capturing :: Stream Bool
capturing = Out ("capturing", expr)
  where
    hasCapt = elemL (iValInt 45) (iNow events_within)
    expr    = iOr hasCapt (iOffset open_capture (-1) (iValBool False))

all_ok_capture :: Stream Bool
all_ok_capture = Out ("all_ok_capture", expr)
  where
    capt = iNow capturing
    ok   = foldr (\ s -> iAnd (iNow s)) (iNow height_ok) [near, roll_ok, pitch_ok]
    expr = iImpl capt ok
 
-- Output stream: flying_in_safe_zones

no_fly :: Stream [Point2]
no_fly = Out ("no_fly", expr)
  where
    expr = iOffset no_fly (-1) (iNow nofly)

flying_in_safe_zones :: Stream Bool
flying_in_safe_zones = Out ("flying_in_safe_zones", expr)
  where
    filPos = iPoint2D (iNow filtered_pos_xp) (iNow filtered_pos_yp)
    poly   = iNow no_fly
    expr   = iNot (pointInPoly filPos poly)
 
-- Output stream: depth_into_poly

depth_into_poly :: Stream (Maybe Double)
depth_into_poly = Out ("depth_into_poly", expr)
  where
    filPos       = iPoint2D (iNow filtered_pos_xp) (iNow filtered_pos_yp)
    poly         = iNow no_fly
    polySides    = polygonSides poly
    shortestDist = foldrL
                     (\ seg n -> iDMin n (iDistancePointSeg filPos seg))
                     (iDistancePointSeg filPos (iHead polySides))
                     (iTail polySides)
    
    expr   = iIte (pointInPoly filPos poly)
                  (iJustM shortestDist)
                  (iNothingM (iValDouble 0))

-- | Specification

spec :: Specification
spec = [ -- input streams
         toDynStrm attitude
       , toDynStrm velocity
       , toDynStrm position
       , toDynStrm altitude
       , toDynStrm target
       , toDynStrm nofly
       , toDynStrm events_within
       
         -- auxiliar streams
       , toDynStrm filtered_pos_xp
       , toDynStrm filtered_pos_yp
       , toDynStrm filtered_pos_alt
       , toDynStrm instantN
         
         -- output stream: all_ok_capture 
       , toDynStrm roll_ok 
       , toDynStrm pitch_ok
       , toDynStrm height_ok
       , toDynStrm near 
       , toDynStrm capturing
       , toDynStrm open_capture 
       , toDynStrm all_ok_capture
       
         -- output stream: flying_in_safe_zones
       , toDynStrm no_fly
       , toDynStrm flying_in_safe_zones
       
         -- output stream: depth_into_poly
       , toDynStrm depth_into_poly ]
