{-# LANGUAGE MultiWayIf,RankNTypes #-}

module Actions where

import Dice
import Types

import Control.Lens
import Control.Monad.State
import Data.Maybe
import System.Random

demote :: CheckRes -> CheckRes
demote res = toEnum $ max 0 (fromEnum res -1)

promote :: CheckRes -> CheckRes
promote res = toEnum $ min 3 (fromEnum res -1)

passes :: CheckRes -> Bool
passes = (>=2) . fromEnum

roll :: Dice -> PF2E Int
roll = lift . rollIO

check :: Int -> Int -> PF2E CheckRes
check bon dc = do
  dieRoll <- roll d20
  let score = dieRoll + bon
      dif = score - dc
      res = if | dif <= -10 -> CritFail
               | dif <  0   -> Fail
               | dif < 10   -> Suc
               | otherwise  -> CritSuc
  return $ case dieRoll of
    1  -> demote res
    20 -> promote res
    _  -> res

tryTumble :: Creature -> Creature -> PF2E Bool
tryTumble tumbling tumbled = if tumbling ^. team  == tumbled ^. team
                                then return True
                                else passes <$> check (tumbling ^. acrobatics) (tumbled ^. refDC)


doAction :: CUID -> Action -> PF2E ()

doAction cid Move{moveActions=actions,movePath=path} = do
  cre <- lookupCre cid
  guard $ and $ zipWith neighbor (cre ^. location:path) path
  tumbleCount  <- length . catMaybes <$> mapM use [squares . at loc | loc <- path ]
  guard $ length path + tumbleCount <= actions * ( cre^.speed `div` 5 )
  doMoveHelp cid path

doAction cid Step{stepDest=dest} = do
  cre <- lookupCre cid
  guard $ neighbor (cre ^. location) dest
  Nothing <- use $ squares . at dest
  creTP cid dest

doAction cid Strike{strikeIndex=i,strikeTarget=t} = do
  cre <- lookupCre cid
  let attack = (cre ^. attacks) !! i
  mapen' <- use mapen :: PF2E Int
  let (b0,b1,b2) = attack ^. bonus :: (Int,Int,Int)
      bonus' = case mapen' of
                 0 -> b0
                 1 -> b1
                 2 -> b2
                 _ -> error "invalid multi attack penalty"
  Just targetCid <- use $ squares . at t
  target <- lookupCre targetCid
  res <- check bonus' (target ^. ac)
  case attack ^. ammoType of
    Just aType -> do
      let mLeft = cre ^. ammo . at aType
      guard $ isJust mLeft
      let left = fromJust mLeft
      guard $ left > 0
      cresById . ix cid . ammo . ix aType -= 1
    Nothing -> return ()
  let maybeDamage = case res of
                 CritSuc -> Just $ attack ^. critDmg
                 Suc     -> Just $ attack ^. dmg
                 _       -> Nothing
  mapM_ (`dealDamage` targetCid) maybeDamage

doAction cid DropProne = do
  cresById . at cid . _Just . prone .= True

doAction cid Stand = do
  cresById . at cid . _Just . prone .= False

doAction cid Escape = do
  cre <- lookupCre cid
  case cre ^. grappledBy of
    Just (grapplerID,dc) -> do
        let bon = (cre ^. athletics) `max` (cre ^. acrobatics) `max` (cre ^. unarmed) :: Int
        res <- check bon dc
        when (passes res) $ cresById . ix cid . grappledBy .= Nothing
    Nothing -> error "escape acction used by un grappled creature"

doAction cid Grapple{ grapTarget = targetSq } = do
  cre <- lookupCre cid
  Just targetId <- use $ squares . at targetSq
  target   <- lookupCre targetId
  res <- check (cre ^. athletics) (target ^. fortDC)
  when (passes res) $ cresById . ix targetId . grappledBy .= Just (cid,10 + cre ^. athletics)

doAction cid Demoralize{ demoralizeTarget = targetSq } = do
  cre <- lookupCre cid
  Just targetId <- use $ squares . at targetSq
  target <- lookupCre targetId
  guard $ targetId `notElem` (cre ^. demoralizeCooldowns )
  res <- check (cre ^. intimidate) (target ^. willDC)
  case res of
    CritFail -> return ()
    Fail     -> return ()
    Suc      -> cresById . ix targetId . frightened %= max 1
    CritSuc  -> cresById . ix targetId . frightened %= max 2

dealDamage :: Damage -> CUID -> PF2E ()
dealDamage (dType,dDice) cid = do
  dVal <- roll dDice
  cre  <- lookupCre cid
  let def  = cre ^. defenses . at dType
  let dVal' = appDef def dVal
  cresById . at cid . _Just . hp -= dVal'

appDef :: Maybe DefenseType -> Int -> Int
appDef Nothing           = id
appDef (Just Immune)     = const 0
appDef (Just (Resist r)) = max 0 . (`subtract` 4)
appDef (Just (Vuln   v)) = (+v)

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

-- should evantually take info about step vs move
-- for reactions
creTP :: CUID -> Square -> PF2E ()
creTP cid dest = do
  cre <- lookupCre cid
  let src = cre ^. location
  let cre' = cre & location .~ dest
  squares  . at src  .= Nothing
  squares  . at dest .= Just cid
  cresById . at cid  .= Just cre'












