module CoverTest where

import Types
import Actions
import Assets.Tools
import Control.Monad.State
import Assets.Defs

coverTest :: PF2E ()
coverTest = do
  loadFile "big"
  c <- determineCover (0,0) (0,7)
  lift $ print c

getTestWorld :: IO World
getTestWorld = execStateT (loadFile "big") defWorld

main :: IO ()
main = evalStateT coverTest defWorld
