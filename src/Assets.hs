module Assets where

import Actions
import Attacks
import Creature
import Dice
import World

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

defWorld :: World
defWorld = World{
  _squares         = M.empty,
  _cresById        = M.empty,
  _globalInititive = [],
  _nextCuid        = 0,
  _actionsLeft     = 3,
  _mapen           = 0
                }

defGob :: Creature
defGob = Creature{
  _cuid       = error "id not set",
  _team       = error "team not set",
  _initiative = 2, -- perception as default init skill
  _hp         = 6,
  _ac         = 16,
  _speed      = 25,
  _frightened = 0,
  _location   = error "location not set",
  _attacks    = [ Attack{
                  _bonus   = (8,4,0),
                  _dmg     = (S,d 6),
                  _critDmg = (S,2 * d 6),
                  _ammo    = Nothing
                        }
                , Attack{
                  _bonus   = (8,3,-2),
                  _dmg     = (P,d 6),
                  _critDmg = (P,2 * d 6 + d 10),
                  _ammo    = Just 10
                        } ],
  _refDC      = 17,
  _athletics  = 2,
  _acrobatics = 5,
  _intimidate = 1,
  _demoralizeCooldowns = S.empty,
  _creatureSpecific = Goblin
                 }

register :: Creature -> PF2E CUID
register cre = do
  cid <- getNextCUID
  cresById . at cid .= Just (cre & cuid .~ cid)
  return cid

place :: CUID -> Square -> PF2E ()
place cid sq = do
  squares . at sq .= Just cid
  cresById . at cid . _Just . location .= sq


rollInitCre :: Creature -> PF2E Int
rollInitCre cre = do
  dieRoll <- roll d20
  return $ dieRoll + cre ^. initiative

rollInit :: PF2E ()
rollInit = do
  cres      <- map snd . M.toList <$> use cresById
  initrolls <- mapM rollInitCre cres
  globalInititive .= ( zip cres initrolls & sortOn snd .> map (fst .> _cuid) )

init2Gob :: PF2E ()
init2Gob = do
  g1 <- register defGob
  cresById . at g1 . _Just . team .= 1
  place g1 (0,0)
  g2 <- register defGob
  cresById . at g2 . _Just . team .= 2
  place g2 (0,1)
  rollInit


