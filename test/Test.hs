{-# LANGUAGE RankNTypes #-}

import Assets.Tools
import Types
import Display
import Run
import AIS

import Control.Lens
import Graphics.Gloss.Interface.IO.Game
import System.Environment

  {-
testTumbleBy :: IO World
testTumbleBy = execStateT (do
  init2Gob
  doAction 0 Move{moveActions=1,movePath=[(0,1),(0,2)]}
    ) defWorld
    -}

vs :: String -> String -> String -> IO World
vs ai1 ai2 w = execPF2E (do
  loadFile w
  loadAI ai1 1
  loadAI ai2 2
                            ) undefined

  {-
get2GobCLI :: IO World
get2GobCLI = vs "simple" "cli" "big"
-}

vsFromArgs :: IO World
vsFromArgs = do
  [ai1,ai2,w] <- getArgs
  vs ai1 ai2 w

  {-
testGraphics :: IO ()
testGraphics = do
    w <- get2GobCLI
    rd <- loadRenderData w
    -- print rd
    playIO FullScreen (makeColor 0 0 0 1) 30 rd renderAll handleMouse tick
    -}

--eventHandle :: Event -> RenderData -> IO RenderData
--eventHandle _event = return

tick :: Float -> RenderData -> IO RenderData
tick _duration rd = do
  let w  = rd ^. world
  let aw = rd ^. worldAsync
  (w',aw') <- stepPF2E step (w,aw)
  return $ rd & world .~ w' & worldAsync .~ aw'

main :: IO ()
main = do
    w <- vsFromArgs
    rd <- loadRenderData w
    playIO FullScreen (makeColor 0 0 0 1) 30 rd renderAll handleMouse tick
