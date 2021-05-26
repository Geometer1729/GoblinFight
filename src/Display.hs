{-# LANGUAGE
  TemplateHaskell
#-}
module Display where

import Types

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Environment
import Control.Lens
import Data.Maybe

import qualified Data.Map as M

data RenderData = RenderData {
                    _world       :: World,
                    _battlefield :: [Square],
                    _grassPic    :: Picture,
                    _gobPic      :: Picture,
                    _screenWidth :: Int,
                    _screenHeight :: Int
                  }

makeLenses ''RenderData

getRectGrid :: M.Map Square a -> [Square]
getRectGrid sMap = let squares = M.keys sMap
                       xmin = minimum $ fst <$> squares
                       xmax = maximum $ fst <$> squares
                       ymin = minimum $ snd <$> squares
                       ymax = maximum $ snd <$> squares
                     in [(x,y) | x <- [xmin..xmax], y <- [ymin..ymax]]


loadRenderData :: World -> IO RenderData
loadRenderData w = do
    let rectgrid = getRectGrid (w ^. squares)
    grassMaybe <- loadJuicyPNG "res/grass.png"
    gobMaybe <- loadJuicyPNG "res/gob.png"
    (width, height) <- getScreenSize
    return RenderData{
        _world = w,
        _battlefield = rectgrid,
        _grassPic = fromJust grassMaybe,
        _gobPic = fromJust gobMaybe,
        _screenWidth = width,
        _screenHeight = height
    }


renderBackground :: RenderData -> IO Picture
renderBackground rd = let w = fromIntegral $ rd ^. screenWidth
                          h = fromIntegral $ rd ^. screenHeight
                          empty = Polygon [(0,0),(w,0),(w,h),(0,h)]
                          background = Color (makeColor 0.2 0.2 0.2 1.0) empty
                      in return background
