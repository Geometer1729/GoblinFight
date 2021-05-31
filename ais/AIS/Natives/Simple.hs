module AIS.Natives.Simple where

import Types
import Actions(linf)

import Flow
import Control.Lens hiding ((.>))
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Data.List

simple :: World -> Action
simple w = let
  cid = w ^. initTracker . to head
  Just cre = w ^. cresById . at cid
  loc = cre ^. location
  as = posibleAttacks w
    in case cre ^. grappledBy of
         Just _ -> Escape
         Nothing -> case as of
           (a:_) -> a
           [] -> case closestEnemy w of
                  Just esq -> moveToward w esq
                  Nothing  -> Stand
 -- if there are no surviving enemies burn actions

moveToward :: World -> Square -> Action
moveToward w dest = let
  cid = w ^. initTracker . to head
  Just cre = w ^. cresById . at cid
  loc = cre ^. location
  spaces = (cre ^. speed) `div` 5
  path = genPath spaces loc dest
    in Move{moveActions=1,movePath=path}

genPath :: Int -> Square -> Square -> [Square]
genPath 0 _ _ = []
genPath n (x,y) dest@(x',y') = let
  dx = case compare x x' of
         LT -> 1
         EQ -> 0
         GT -> -1
  dy = case compare y y' of
         LT -> 1
         EQ -> 0
         GT -> -1
  next = (x+dx,y+dy)
        in if next == dest
              then []
              else next:genPath (n-1) next dest

posibleAttacks :: World -> [Action]
posibleAttacks w = do
  let cid = w ^. initTracker . to head
  let cre = fromJust $ w ^. cresById . at cid
  (attack,attackIndex)  <- zip (cre ^. attacks) [0..]
  let r  = attack ^. range
  targetSq <- addPair (cre ^. location ) <$> inRange r
  targetId <- maybeToList $ w ^. squares . at targetSq
  target <-   maybeToList $ w ^. cresById . at targetId
  guard $ target ^. team /= cre ^. team -- friendly fire is cringe
  guard $ hasAmmo cre attack
  return $ Strike{strikeIndex=attackIndex,strikeTarget=targetSq}

hasAmmo :: Creature -> Attack -> Bool
hasAmmo cre atk = case atk ^. ammoType of
                    Nothing -> True
                    Just ammoT -> case cre ^. ammo . at ammoT of
                                    Nothing -> False
                                    Just ammount -> ammount > 0


inRange :: Range -> [Square]
inRange r' = let
  r = maxRange r' `div` 5
    in [ (x,y) | x <- [-r..r] , y <- [-r..r] , (x,y) /= (0,0) ]

maxRange :: Range -> Int
maxRange (Simple n) = n
maxRange (Increment n) = 6*n

closestEnemy :: World -> Maybe Square
closestEnemy w = let
  cid = w ^. initTracker . to head
  cre = fromJust $ w ^. cresById . at cid
  loc = cre ^. location
  seEnemies :: [Square]
  seEnemies = do
    (sq,tid) <- M.toList $ w ^. squares
    let Just target = w ^. cresById . at tid
    guard  $ target ^. team /= cre ^. team
    return sq
  sorted = seEnemies & sortOn (linf loc)
    in listToMaybe sorted

