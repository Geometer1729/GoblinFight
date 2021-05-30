{-# LANGUAGE
  MultiWayIf
 ,RankNTypes
#-}

module Actions where

import Dice
import Types

import Control.Lens hiding ((.>))
import Control.Monad.State
import Data.Maybe
import System.Random
import Data.Functor

import qualified Data.Map as M

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

doAction :: CUID -> Action -> PF2E ()
doAction cid Move{moveActions=actions,movePath=path} = do
  actionsLeft -= actions
  cre <- lookupCre cid
  guard $ and $ zipWith neighbor (cre ^. location:path) path
  tumbleCount  <- length . catMaybes <$> mapM use [squares . at loc | loc <- path ]
  guard $ length path + tumbleCount <= actions * ( cre^.speed `div` 5 )
  doMoveHelp cid path

doAction cid Step{stepDest=dest} = do
  actionsLeft -= 1
  cre <- lookupCre cid
  guard $ neighbor (cre ^. location) dest
  Nothing <- use $ squares . at dest
  creTP cid dest

doAction cid Strike{strikeIndex=i,strikeTarget=t} = do
  actionsLeft -= 1
  cre <- lookupCre cid
  let attack = (cre ^. attacks) !! i
  mapen' <- use mapen :: PF2E Int
  mapen += 1
  let bonus' = attack ^. bonus . mapLen mapen'
  Just targetCid <- use $ squares . at t
  target <- lookupCre targetCid
  case attack ^. ammoType of
    Just aType -> do
      let mLeft = cre ^. ammo . at aType
      guard $ isJust mLeft
      let left = fromJust mLeft
      guard $ left > 0
      cresById . ix cid . ammo . ix aType -= 1
    Nothing -> return ()
  let r = linf (cre ^. location) (target ^. location)
  rangePen <- case attack ^. range of
                 Simple ar -> guard (r < ar) $> 0
                 Increment ri -> guard (r < 6 * ri) $> 2 * (r `div` ri)

  isFlanking <- checkFlanking cid targetCid
  let targFlatFooted = isJust (target ^. grappledBy ) || target ^. prone || isFlanking
      tac = (target ^. wF ac) - if targFlatFooted then 2 else 0
  res <- check (bonus' - rangePen - cre ^. frightened) tac
  let maybeDamage = case res of
                 CritSuc -> Just $ attack ^. critDmg
                 Suc     -> Just $ attack ^. dmg
                 _       -> Nothing
  mapM_ (`dealDamage` targetCid) maybeDamage

doAction cid DropProne = do
  actionsLeft -= 1
  cresById . at cid . _Just . prone .= True

doAction cid Stand = do
  actionsLeft -= 1
  cresById . at cid . _Just . prone .= False

doAction cid Escape = do
  actionsLeft -= 1
  cre <- lookupCre cid
  case cre ^. grappledBy of
    Just grapplerID -> do
        let bon = (cre ^. wF athletics) `max` (cre ^. wF acrobatics) `max` (cre ^. wF unarmed) :: Int
        grapler <- lookupCre grapplerID
        res <- check bon (grapler ^. grappleDC)
        when (passes res) $ cresById . ix cid . grappledBy .= Nothing
    Nothing -> error "escape acction used by un grappled creature"

doAction cid Grapple{ grapTarget = targetSq } = do
  actionsLeft -= 1
  cre <- lookupCre cid
  Just targetId <- use $ squares . at targetSq
  target   <- lookupCre targetId
  guard $ neighbor (cre ^. location) (target ^. location)
  res <- check (cre ^. wF athletics) (target ^. wF fortDC)
  when (passes res) $ cresById . ix targetId . grappledBy .= Just cid

doAction cid Demoralize{ demoralizeTarget = targetSq } = do
  actionsLeft -= 1
  cre <- lookupCre cid
  Just targetId <- use $ squares . at targetSq
  target <- lookupCre targetId
  guard $ targetId `notElem` (cre ^. demoralizeCooldowns )
  res <- check (cre ^. wF intimidate) (target ^. wF willDC)
  case res of
    CritFail -> return ()
    Fail     -> return ()
    Suc      -> cresById . ix targetId . frightened %= max 1
    CritSuc  -> cresById . ix targetId . frightened %= max 2

mapLen :: Int -> Lens' (Int,Int,Int) Int
mapLen 0 = _1
mapLen 1 = _2
mapLen _ = _3

tryTumble :: Creature -> Creature -> PF2E Bool
tryTumble tumbling tumbled = if tumbling ^. team  == tumbled ^. team
                                then return True
                                else passes <$> check (tumbling ^. wF acrobatics) (tumbled ^. wF refDC)

checkFlanking :: CUID -> CUID -> PF2E Bool
checkFlanking cid1 cid2 = do
  c1 <- lookupCre cid1
  c2 <- lookupCre cid2
  let l1 = c1 ^. location
      l2 = c2 ^. location
      fos = flankOptions l1 l2
  flankerids <- catMaybes <$> mapM (use . fmap squares . at) fos :: PF2E [CUID]
  flankers <- mapM lookupCre flankerids
  return $ any (\c -> c ^. team == c1 ^. team) flankers

flankOptions :: Square -> Square -> [Square]
flankOptions (x1,y1) (x2,y2) = let
  dx = x2 - x1
  dy = y2 - y1
    in [(x2+dx,y2+dx)]
-- This will evantually be updated to handle reach
-- and large+ creatures correctly
-- that's why it uses the list type even though it always returns 1 square

dealDamage :: Damage -> CUID -> PF2E ()
dealDamage (dType,dDice) cid = do
  dVal <- roll dDice
  cre  <- lookupCre cid
  let def  = cre ^. defenses . at dType
  let dVal' = appDef def dVal
  cresById . at cid . _Just . hp -= dVal'
  cre' <- lookupCre cid -- new cre post damage
  let dead = cre' ^. hp < 0
  when dead $ removeCre cid

removeCre :: CUID -> PF2E ()
removeCre cid = do
  cresById . at cid .= Nothing
  squares         %= M.filter (/= cid)
  globalInititive %=   filter (/= cid)
  cresById . each . grappledBy %= filterMaybe (/= cid)

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe pred (Just a)
  | pred a = Just a
filterMaybe _ _ = Nothing
--filterMaybe pred m = m >>= guard . pred >> m
-- works but seems less readable

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

linf :: Square -> Square -> Int
linf (x1,y1) (x2,y2) = 5 * max (abs (x1-x2)) (abs (y1-y2))

neighbor :: Square -> Square -> Bool
neighbor a b = linf a b == 5

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

-- with fright
wF :: Lens' Creature Int -> Getter Creature Int
wF l = to $ \cre -> cre ^. l - cre ^. frightened

grappleDC :: Getter Creature Int
grappleDC = to $ \cre -> 10 + cre ^. athletics - cre ^. frightened

