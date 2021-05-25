{-# LANGUAGE
  TemplateHaskell
#-}

module World where

import Creature
import Control.Lens

import qualified Data.Map as M

data World = World{
   _squares         :: M.Map Square CUID,
   _cresById        :: M.Map CUID Creature,
   _globalInititive :: [CUID],
   _nextCuid        :: CUID,
   _actionsLeft     :: Int,
   _mapen           :: Int  -- 0 1 or 2 (rather than 0 5 or 10)
    } deriving Show

makeLenses ''World
