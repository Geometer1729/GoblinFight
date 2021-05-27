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

--all textures stored on disk as 512x512 pixels
data RenderData = RenderData {
                    _world       :: World,
                    _battlefield :: [Square],
                    _grassPic    :: Picture,
                    _gobPic      :: Picture,
                    _screenWidth :: Int,
                    _screenHeight :: Int,
                    _globalZoom  :: Int,
                    _globalPan   :: (Int,Int),
                    _defaultImageSize :: Int
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
        _screenHeight = height,
        _globalZoom = 32,
        _globalPan = (0,0),
        _defaultImageSize = 512
    }


renderBackground :: RenderData -> IO Picture
renderBackground rd = let w = fromIntegral $ rd ^. screenWidth
                          h = fromIntegral $ rd ^. screenHeight
                          empty = Polygon [(0,0),(w,0),(w,h),(0,h)]
                          background = Color (makeColor 0.2 0.2 0.2 1.0) empty
                      in return background

renderGrass :: RenderData -> IO Picture
renderGrass rd = do
    let tilesize = rd ^. globalZoom
    let (gx,gy) = rd ^. globalPan
    let scaleFactor = (fromIntegral $ rd ^. globalZoom) / (fromIntegral $ rd ^. defaultImageSize)
    let grassRenders = [Translate (fromIntegral (x * tilesize + gx) ) (fromIntegral (y * tilesize + gy)) (Scale scaleFactor scaleFactor (rd ^. grassPic)) | (x,y) <- (rd ^. battlefield)]
    return $ Pictures grassRenders

getTeamColor :: Int -> Color
getTeamColor n = let
  tau = 2*pi
  phi = (1+sqrt 5)/2
  coolAngle = tau/phi
  angle = fromIntegral n * coolAngle
    in makeColor (cos angle) (cos angle+tau/3) (cos angle-tau/3) 1

renderGoblins :: RenderData -> IO Picture
renderGoblins rd = do
   let gobs = M.elems $ rd ^. (world . cresById)
   return undefined
