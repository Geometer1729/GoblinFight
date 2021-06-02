module Assets.Tools where

import Actions
import Dice
import Types
import Assets.Defs (defWorld)
import Assets.PWParser

import Control.Lens hiding ((.>))
import Control.Monad.State
import Flow
import Data.List

import qualified Data.Set as S
import qualified Data.Map as M

getNextCUID :: PF2E CUID
getNextCUID = do
  cid <- use nextCuid
  nextCuid += 1
  return cid

register :: Creature -> PF2E CUID
register cre = do
  cid <- getNextCUID
  cresById . at cid .= Just (cre & cuid .~ cid)
  return cid

place :: CUID -> Square -> PF2E ()
place cid sq = do
  squares . at sq .= Just cid
  battlefield %= S.insert sq
  cresById . at cid . _Just . location .= sq

placeSquare :: Square -> PF2E ()
placeSquare sq = do
    battlefield %= S.insert sq

placeRect :: Square -> Square -> PF2E ()
placeRect (x1,y1) (x2,y2) = mapM_ placeSquare [ (x,y) | x<-[x1..x2] , y<- [y1..y2] ]

rollInitCre :: Creature -> PF2E Int
rollInitCre cre = do
  dieRoll <- roll d20
  return $ dieRoll + cre ^. initiative

rollInit :: PF2E ()
rollInit = do
  creatures <- map snd . M.toList <$> use cresById
  initrolls <- mapM rollInitCre creatures
  initTracker .= ( zip creatures initrolls & sortOn snd .> map (^. _1 . cuid) )

initCre :: Creature -> Int -> Square -> PF2E ()
initCre cre t sq = do
  cid <- register cre
  cresById . at cid . each . team .= t
  place cid sq

loadPW :: PreWorld -> PF2E ()
loadPW pw = do
  put defWorld
  sequence_ $ do
    (t,members) <- pw ^. cres
    (mem,sq) <- members
    return $ initCre mem t sq
  sequence_ $ do
    (l,r) <- pw ^. rects
    return $ placeRect l r
  sequence_ $ do
    sq <- pw ^. sqs
    return $ placeSquare sq
  rollInit

loadFile :: String -> PF2E ()
loadFile path = do
  pw <- liftIO $ parsePw <$> readFile ("worlds/" ++ path)
  loadPW pw
