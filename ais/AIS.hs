{-# OPTIONS_GHC -pgmF ./ais/Preproc -F #-}
{-# LANGUAGE TemplateHaskell #-}

module AIS where

--preproc native imports

import Types
import Control.Lens
import AIS.THLoaders

import qualified Data.Map as M

aiMap :: M.Map String AI
aiMap = M.union natives execs

natives :: M.Map String AI
natives = M.fromList ( $(loadNatives) & each . _2 %~ Native )

execs :: M.Map String AI
execs = M.fromList $(loadExecs)
