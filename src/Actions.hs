{-# LANGUAGE MultiWayIf,RankNTypes #-}

module Actions where

import Creature
import World

import Control.Lens
import Control.Monad.State
import Data.Maybe
import System.Random

data Action =
    Move {moveActions :: Int , movePath :: [Square] }
  | Step {stepDest :: Square }
  | Strike { strikeIndex :: Int, strikeTarget :: Square }
  | Aid{ aidTarget :: Square }
  | DropProne
  | Stand
  | Escape
  | Grapple{ grapTarget :: Square }
  | Demoralize{ demoralizeTarget :: Square }

type PF2E = StateT World IO
type Stat = Lens' Creature Int

data CheckRes = CritFail | Fail | Suc | CritSuc deriving(Enum)

demote :: CheckRes -> CheckRes
demote res = toEnum $ max 0 (fromEnum res -1)

promote :: CheckRes -> CheckRes
promote res = toEnum $ min 3 (fromEnum res -1)

passes :: CheckRes -> Bool
passes = (>=2) . fromEnum

d20 :: PF2E Int
d20 = lift $ randomRIO (1,20)

checkVS :: Creature -> Stat -> Creature -> Stat -> PF2E CheckRes
checkVS creCheck skill creDC dcStat = do
  roll <- d20
  let dc    = creDC ^. dcStat
  let score = roll + creCheck ^. skill
  let dif = score - dc
  let res = if | dif <= -10 -> CritFail
               | dif <  0   -> Fail
               | dif < 10   -> Suc
               | otherwise  -> CritSuc
  return $ case roll of
    1  -> demote res
    20 -> promote res
    _  -> res

tryTumble :: Creature -> Creature -> PF2E Bool
tryTumble tumbling tumbled = if tumbling ^. team  == tumbled ^. team
                                then return True
                                else passes <$> checkVS tumbling acrobatics tumbled refDC

doAction :: CUID -> Action -> PF2E ()
doAction cid Move{moveActions=actions,movePath=path} = do
  cre <- lookupCre cid
  guard $ and $ zipWith neighbor (cre ^. location:path) path
  tumbleCount  <- length . catMaybes <$> mapM use [squares . at loc | loc <- path ]
  guard $ length path + tumbleCount <= actions * ( cre^.speed `div` 5 )
  doMoveHelp cid path
doAction cid Step{} = undefined cid
doAction _ _ = undefined

doMoveHelp :: CUID -> [Square] -> PF2E ()
doMoveHelp _ [] = return ()
doMoveHelp cid (dest:rest) = do
  maybeTumbleID <- use $ squares . at dest :: PF2E (Maybe CUID)
  cre <- lookupCre cid
  case maybeTumbleID of
    Just tumbledID -> do
      tumbled <- lookupCre tumbledID
      res <- tryTumble cre tumbled
      when res $ doMoveHelp cid rest
    Nothing -> creTP cid dest >> doMoveHelp cid rest

lookupCre :: CUID -> PF2E Creature
lookupCre cid = do
    maybeCre <- use $ cresById . at cid
    case maybeCre of
      Just cre -> return cre
      Nothing  -> error "lookup given invalid creature id"

neighbor :: Square -> Square -> Bool
neighbor (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2)) == 1

creTP :: CUID -> Square -> PF2E ()
creTP cid dest = do
  cre <- lookupCre cid
  let src = cre ^. location
  let cre' = cre & location .~ dest
  squares  . at src  .= Nothing
  squares  . at dest .= Just cid
  cresById . at cid  .= Just cre'












