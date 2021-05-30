{-# LANGUAGE
  MultiWayIf
 ,RankNTypes
#-}

module Actions where

import Dice
import Types

import Control.Applicative
import Control.Lens hiding ((.>))
import Control.Monad.State
import Data.Functor
import Data.Maybe
import Flow
import System.Random

import qualified Data.Map as M
import qualified Data.Set as S

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
  guard ( isNothing (cre ^. grappledBy) ) <|> fail "move attempt from grappled creature"
  guard ( and $ zipWith neighbor (cre ^. location:path) path ) <|> fail "invalid move path"
  tumbleCount  <- length . catMaybes <$> mapM use [squares . at loc | loc <- path ]
  guard ( length path + tumbleCount <= actions * ( cre^.speed `div` 5 ) ) <|> fail "move path too long"
  doMoveHelp cid path

doAction cid Step{stepDest=dest} = do
  actionsLeft -= 1
  cre <- lookupCre cid
  guard ( neighbor (cre ^. location) dest ) <|> fail "invalid step"
  Nothing <- use $ squares . at dest
  creTP cid dest

doAction cid Strike{strikeIndex=i,strikeTarget=t} = do
  actionsLeft -= 1
  cre <- lookupCre cid
  guard (not $ any fst (cre ^. grappledBy)) <|> fail "can't strike while restrained"
    -- any is folding the maybe so only if you are grappled and it was a crit are you restrained
  let attack = (cre ^. attacks) !! i
  mapen' <- use mapen :: PF2E Int
  mapen += 1
  let bonus' = attack ^. bonus . mapLen mapen'
  Just targetCid <- use $ squares . at t
  target <- lookupCre targetCid
  case attack ^. ammoType of
    Just aType -> do
      let mLeft = cre ^. ammo . at aType
      guard ( isJust mLeft ) <|> fail "ammo type lookup fail"
      let left = fromJust mLeft
      guard (left > 0) <|> fail "no ammo left to fire that"
      cresById . ix cid . ammo . ix aType -= 1
    Nothing -> return ()
  let r = linf (cre ^. location) (target ^. location)
  rangePen <- case attack ^. range of
                 Simple ar -> ( guard (r <= ar) $> 0 )  <|> fail "can't reach target"
                 Increment ri -> ( guard (r <= 6 * ri) $> 2 * (r `div` ri) )
                      <|> fail " targetmore than 6 range incriments out"

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
    Just (_,grapplerID) -> do
        let bon = (cre ^. wF athletics) `max` (cre ^. wF acrobatics) `max` (cre ^. wF unarmed) :: Int
        grapler <- lookupCre grapplerID
        res <- check bon (grapler ^. grappleDC)
        when (passes res) $ cresById . ix cid . grappledBy .= Nothing
    Nothing -> fail "escape acction used by un grappled creature"

doAction cid Grapple{ grapTarget = targetSq } = do
  actionsLeft -= 1
  cre <- lookupCre cid
  guard (isNothing $ cre ^. grappling) <|> fail "you're already grapling someone"
  Just targetId <- use $ squares . at targetSq
  target   <- lookupCre targetId
  guard ( neighbor (cre ^. location) (target ^. location) ) <|> fail "grappled non-adjacent square"
  -- creatures should have inate reach for grapple
  res <- check (cre ^. wF athletics) (target ^. wF fortDC)
  let crits = res == CritSuc
  when (passes res) $ do
    cresById . ix targetId . grappledBy .= Just (crits,cid)
    cresById . ix cid      . grappling  .= Just (crits,targetId)

doAction cid Release = do
  cre <- lookupCre cid
  let Just (_,targetId) = cre ^. grappling
  cresById . ix targetId . grappledBy .= Nothing
  cresById . ix cid      . grappling  .= Nothing

doAction cid Demoralize{ demoralizeTarget = targetSq } = do
  actionsLeft -= 1
  cre <- lookupCre cid
  Just targetId <- use $ squares . at targetSq
  target <- lookupCre targetId
  guard ( targetId `notElem` (cre ^. demoralizeCooldowns ) ) <|> fail "demoralize creature in cooldown"
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
  initTracker %=   filter (/= cid)
  cresById . each . grappledBy %= filterMaybe (snd .> (/= cid))

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
  bf <- use battlefield
  guard $ dest `S.member` bf
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
  squares  . at src  .= Nothing
  squares  . at dest .= Just cid
  cresById . ix cid  . location .= dest
  case cre ^. grappling of
    Nothing -> return ()
    Just (_,tid) -> do
      target <- lookupCre tid
      let dist = linf (target ^. location) dest
      when (dist > 1) $ doAction cid Release

-- with fright
wF :: Lens' Creature Int -> Getter Creature Int
wF l = to $ \cre -> cre ^. l - cre ^. frightened

grappleDC :: Getter Creature Int
grappleDC = to $ \cre -> 10 + cre ^. athletics - cre ^. frightened

