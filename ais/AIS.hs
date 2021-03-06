{-# OPTIONS_GHC -pgmF ./ais/Preproc -F #-}
{-# LANGUAGE TemplateHaskell #-}

module AIS where

--preproc native imports

import Types
import Control.Lens
import AIS.THLoaders

import qualified Data.Map as M

loadAI :: String -> Int -> PF2E ()
loadAI "cli"   t = ais . at t .= Just CLI
loadAI "gloss" t = ais . at t .= Just Gloss
loadAI name    t = ais . at t .= aiMap ^. at name

aiMap :: M.Map String AI
aiMap = M.union natives execs

natives :: M.Map String AI
natives = M.fromList [ (name,Native actionai reactionai) | (name,actionai,reactionai) <-  $(loadNatives) ]

execs :: M.Map String AI
execs = M.fromList $(loadExecs)
