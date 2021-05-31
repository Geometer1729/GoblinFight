{-# LANGUAGE
  TemplateHaskell
#-}
module Display where

import Types

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

--all textures stored on disk as 512x512 pixels
data RenderData = RenderData {
                    _world       :: World,
                    _grassPic    :: Picture,
                    _gobPic      :: Picture,
                    _selectPic   :: Picture,
                    _screenWidth :: Int,
                    _screenHeight :: Int,
                    _globalZoom  :: Int,
                    _globalPan   :: (Float,Float),
                    _defaultImageSize :: Int,
                    _leftMouseDown :: Bool,
                    _lastDragPos :: (Float,Float),
                    _selectedSquare :: Maybe Square
                  } deriving (Show)

makeLenses ''RenderData

{-
getRectGrid :: M.Map Square a -> [Square]
getRectGrid sMap = let squares = M.keys sMap
                       xmin = minimum $ fst <$> squares
                       xmax = maximum $ fst <$> squares
                       ymin = minimum $ snd <$> squares
                       ymax = maximum $ snd <$> squares
                     in [(x,y) | x <- [xmin..xmax], y <- [ymin..ymax]]
                     -}

loadRenderData :: World -> IO RenderData
loadRenderData w = centerPan <$> do
    grassMaybe <- loadJuicyPNG "res/grass.png"
    gobMaybe <- loadJuicyPNG "res/gob.png"
    selectMaybe <- loadJuicyPNG "res/select.png"
    (width, height) <- getScreenSize
    return RenderData{
        _world = w,
        _grassPic = fromJust grassMaybe,
        _gobPic = fromJust gobMaybe,
        _selectPic = fromJust selectMaybe,
        _screenWidth = width,
        _screenHeight = height,
        _globalZoom = 128,
        _globalPan = (0,0),
        _defaultImageSize = 512,
        _leftMouseDown = False,
        _lastDragPos = (0,0),
        _selectedSquare = Nothing
    }

centerPan :: RenderData -> RenderData
centerPan rd = let
  bf = S.toList $ rd ^. world . battlefield
  left  = minimum . map fst $ bf
  right = maximum . map fst $ bf
  up    = maximum . map snd $ bf
  down  = minimum . map snd $ bf
  scale = rd ^. globalZoom
  x = negate $ (fromIntegral scale/2) * fromIntegral (left+right)
  y = negate $ (fromIntegral scale/2) * fromIntegral (up  +down )
    in rd & globalPan .~ (x,y)



onSquare :: RenderData -> Picture -> Square -> Picture
onSquare rd p (x,y) =
    let tilesize = rd ^. globalZoom
        (gx,gy) = rd ^. globalPan
        scaleFactor = (fromIntegral $ rd ^. globalZoom) / (fromIntegral $ rd ^. defaultImageSize)
    in Translate (gx + (fromIntegral $ x * tilesize)) (gy + (fromIntegral $ y * tilesize)) (Scale scaleFactor scaleFactor p)

renderGrass :: RenderData -> IO Picture
renderGrass rd = do
    let grassRenders = [onSquare rd (rd ^. grassPic) s | s <- S.toList (rd ^. world . battlefield)]
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

renderGUI :: RenderData -> IO Picture
renderGUI rd | null $ rd ^. selectedSquare = return Blank
             | otherwise = let sq = fromJust (rd ^. selectedSquare)
                           in return $ onSquare rd (rd ^. selectPic) sq

renderAll :: RenderData -> IO Picture
renderAll rd = do
    grass <- renderGrass rd
    gobs <- renderGoblins rd
    overlay <- renderGUI rd
    return $ Pictures [grass, gobs, overlay]


handleMouse :: Event -> RenderData -> IO RenderData
handleMouse (EventKey key state mods (keyx,keyy)) rd =
    case state of
        Up -> keyUp key (keyx,keyy) rd
        Down -> keyDown key (keyx,keyy) rd
handleMouse (EventMotion (mox,moy)) rd = mouseMovement (mox,moy) rd
handleMouse _ rd = return rd


keyUp :: Key -> (Float,Float) -> RenderData-> IO RenderData
keyUp (MouseButton LeftButton) (x,y) rd = trace "mouse up" $ return (rd & leftMouseDown .~ False)
keyUp _ _ rd = return rd

keyDown :: Key -> (Float,Float) -> RenderData -> IO RenderData
keyDown (MouseButton LeftButton) (x,y) rd = do
    return (rd & leftMouseDown .~ True & lastDragPos .~ (x,y))
keyDown (MouseButton RightButton) (x,y) rd = do
        let square = screenToSquare (x,y) rd
            selectrd = if square `S.member` (rd ^. world . battlefield)
                then rd & selectedSquare .~ Just square
                else rd & selectedSquare .~ Nothing
        return selectrd
keyDown _ _ rd = return rd

mouseMovement :: (Float,Float) -> RenderData -> IO RenderData
mouseMovement (x,y) rd =
        let pan_new = if rd ^. leftMouseDown
                        then addPair (rd ^. globalPan) $ addPair (x,y) (negPair (rd ^. lastDragPos))
                        else rd ^. globalPan
        in traceShow (rd ^. globalPan) $ return (rd & globalPan .~ pan_new & lastDragPos .~ (x,y))

screenToSquare :: (Float,Float) -> RenderData -> Square
screenToSquare (x,y) rd =
    let (panx,pany) = rd ^. globalPan
        zoom = fromIntegral $ rd ^. globalZoom
        rx = round $ (x - panx)/zoom
        ry = round $ (y - pany)/zoom
    in (rx,ry)

