{-# LANGUAGE RankNTypes #-}

import Assets.Tools
import Types
import Display
--import Control.Lens
import Run
import AIS

--import Control.Monad.State
--import Control.Monad.Reader
import Graphics.Gloss.Interface.IO.Game

  {-
testTumbleBy :: IO World
testTumbleBy = execStateT (do
  init2Gob
  doAction 0 Move{moveActions=1,movePath=[(0,1),(0,2)]}
    ) defWorld
    -}

get2GobCLI :: IO World
get2GobCLI = execPF2E (do
  loadFile "big"
  loadAI "simple" 1
  loadAI "simple" 2
    )

testGraphics :: IO ()
testGraphics = do
    w <- get2GobCLI
    rd <- loadRenderData w
    print rd
    playIO FullScreen (makeColor 0 0 0 1) 30 rd renderAll handleMouse tick

--eventHandle :: Event -> RenderData -> IO RenderData
--eventHandle _event = return

tick :: Float -> RenderData -> IO RenderData
tick _duration = undefined
  {-
  execStateT $ do
  w  <- use world
  w' <- lift $ execStateT step w
  world .= w'
  -}

main :: IO ()
main = do
    putStrLn "graphics test"
    testGraphics
