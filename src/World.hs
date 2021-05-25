{-# LANGUAGE
  TemplateHaskell
#-}

module World where

import Creature
import Control.Lens

import qualified Data.Map as M

data World = World{
   _squares     :: M.Map Square CUID,
   _cresById    :: M.Map CUID Creature,
   _globalInititive :: [CUID],
   _nextCuid :: CUID
    } deriving Show

makeLenses ''World
