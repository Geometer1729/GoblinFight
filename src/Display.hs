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
                  } deriving (Show)

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
        _globalZoom = 128,
        _globalPan = (0,0),
        _defaultImageSize = 512
    }

onSquare :: RenderData -> Picture -> Square -> Picture
onSquare rd p (x,y) =
    let tilesize = rd ^. globalZoom
        (gx,gy) = rd ^. globalPan
        scaleFactor = (fromIntegral $ rd ^. globalZoom) / (fromIntegral $ rd ^. defaultImageSize)
    in Translate (fromIntegral $ x * tilesize + gx) (fromIntegral $ y * tilesize + gy) (Scale scaleFactor scaleFactor p)

renderGrass :: RenderData -> IO Picture
renderGrass rd = do
    let grassRenders = [onSquare rd (rd ^. grassPic) s | s <- (rd ^. battlefield)]
    return $ Pictures grassRenders

getTeamColor :: Int -> Color
getTeamColor n = let
  tau = 2*pi
  phi = (1+sqrt 5)/2
  coolAngle = tau/phi
  angle = fromIntegral n * coolAngle
  scaledCos x = (cos x + 1) /2
    in makeColor (scaledCos angle) (scaledCos $ angle+tau/3) (scaledCos $ angle-tau/3) 1

renderGoblins :: RenderData -> IO Picture
renderGoblins rd = do
   let gobs = M.elems $ rd ^. (world . cresById)
   let gobsPics = Pictures [onSquare rd (Pictures [rd ^. gobPic, renderGoblinHealthbar rd g]) (g ^. location) | g <- gobs]
   return $ gobsPics

renderGoblinHealthbar :: RenderData -> Creature -> Picture
renderGoblinHealthbar rd g =
    let healthbarWidth = 0.8 * (fromIntegral (rd ^. defaultImageSize))
        healthbarHeight = healthbarWidth / 20
        fullHealth = Color black (rectangleSolid healthbarWidth healthbarHeight)
        teamcolor = getTeamColor (g ^. team)
        maxhp = fromIntegral $ g ^. maxHp
        curhp = fromIntegral $ g ^. hp
        currentHealth = Translate (-((maxhp-curhp)/(2*maxhp))*healthbarWidth) 0 $  Color teamcolor (rectangleSolid (healthbarWidth * (curhp/maxhp)) healthbarHeight)
    in Translate 0 (0.35*(fromIntegral $ rd ^. defaultImageSize)) $ Pictures [fullHealth,currentHealth]

renderAll :: RenderData -> IO Picture
renderAll rd = do
    grass <- renderGrass rd
    gobs <- renderGoblins rd
    return $ Pictures [grass, gobs]
