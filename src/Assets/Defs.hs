module Assets.Defs where

import Types
import Dice

import qualified Data.Set as S
import qualified Data.Map as M

defWorld :: World
defWorld = World{
  _squares         = M.empty,
  _battlefield     = S.empty,
  _cresById        = M.empty,
  _initTracker     = [],
  _nextCuid        = 0,
  _actionsLeft     = 3,
  _mapen           = 0,
  _ais             = M.empty,
  _aiActionAwait   = Nothing,
  _glossTurn       = False
                }

defGob :: Creature
defGob = Creature{
  _cuid       = error "id not set",
  _team       = error "team not set",
  _initiative = 2, -- perception as default init skill
  _hp         = 6,
  _maxHp      = 6,
  _ac         = 16,
  _speed      = 25,
  _frightened = 0,
  _location   = error "location not set",
  _attacks    = [ Attack{
                  _bonus    = (8,4,0),
                  _dmg      = (S,d6),
                  _critDmg  = (S,2 d6),
                  _ammoType = Nothing,
                  _range    = Simple 5
                        }
                , Attack{
                  _bonus    = (8,3,-2),
                  _dmg      = (P,d6),
                  _critDmg  = (P,2 d6 + d10),
                  _ammoType = Just "arrow",
                  _range    = Increment 20
                        } ],
  _prone      = True,
  _ammo       = M.fromList [("arrow",10)],
  _ref        = 7,
  _fort       = 5,
  _will       = 3,
  _athletics  = 2,
  _acrobatics = 5,
  _intimidate = 1,
  _demoralizeCooldowns = S.empty,
  _defenses   = M.empty,
  _unarmed    = 0,
  _grappledBy = Nothing,
  _grappling  = Nothing,
  _reaction   = True,
  _reactions  = M.fromList [(EndMovementAdjacentAlly,[RStep])],
  _creatureSpecific = Goblin
                 }

