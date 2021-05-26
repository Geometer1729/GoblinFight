module AI where

import Types
import Actions

import System.Process
import Control.Monad.Trans

runAI :: AI -> World -> PF2E Action
runAI (Native f)        w = return $ f w
runAI CLI               w = lift $ print w >> readLn
runAI (Executable path) w = lift $ read <$> readProcess path [] (show w)

