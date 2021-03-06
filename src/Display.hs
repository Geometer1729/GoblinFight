{-# LANGUAGE
  TemplateHaskell
#-}
module Display where

import Types
import Buttons

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

--all textures stored on disk as 512x512 pixels

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
        _worldAsync = Nothing,
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
                               overlay = if (rd ^. world . glossTurn) && shouldRenderOverlay rd sq
                                            then renderOverlay rd sq
                                            else Blank
                           in return $ onSquare rd (Pictures [rd ^. selectPic, overlay]) sq

shouldRenderOverlay :: RenderData -> Square -> Bool
shouldRenderOverlay rd sq =
    let w = rd ^. world
        s = w ^. squares
        cId = M.findWithDefault (-1) sq s
        init = w ^. initTracker
    in head init == cId

renderOverlay :: RenderData -> Square -> Picture
renderOverlay rd sq = renderActionGUI rd

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
keyUp (MouseButton LeftButton) (x,y) rd = return (rd & leftMouseDown .~ False)
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
        in return (rd & globalPan .~ pan_new & lastDragPos .~ (x,y))

screenToSquare :: (Float,Float) -> RenderData -> Square
screenToSquare (x,y) rd =
    let (panx,pany) = rd ^. globalPan
        zoom = fromIntegral $ rd ^. globalZoom
        rx = round $ (x - panx)/zoom
        ry = round $ (y - pany)/zoom
    in (rx,ry)

actionGUI :: BZ Action
actionGUI = Zipper []
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.1 0.2 1 1) "Move",
        buttonInfo = Move{movePath=[]}
    }
    [
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.1 0.2 0.8 1) "Step",
        buttonInfo = Step{stepDest=(0,0)}
    },
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.9 0.2 0.1 1) "Strike",
        buttonInfo = Strike{strikeIndex=(-1),strikeTarget=(0,0)}
    },
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.4 0.4 0.5 1) "Drop Prone",
        buttonInfo = DropProne
    },
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.4 0.4 0.5 1) "Stand",
        buttonInfo = Stand
    },
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.4 0.4 0.5 1) "Escape",
        buttonInfo = Escape
    },
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.2 1 0.4 1) "Grapple",
        buttonInfo = Grapple{grapTarget=(0,0)}
    },
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.4 0.4 0.5 1) "Release",
        buttonInfo = Release
    },
    Button {
        buttonPic = getButtonImage 200 40 (makeColor 0.4 0.4 0.5 1) "Demoralize",
        buttonInfo = Demoralize{demoralizeTarget=(0,0)}
    }
    ]

--Only called when square is selected
renderActionGUI ::  RenderData -> Picture
renderActionGUI rd =
    let (Zipper leftbuttons selected rightbuttons) = actionGUI
        sq = fromJust $ rd ^. selectedSquare
        squareSize =  fromIntegral $ rd ^. globalZoom
        --render left of zipper
        leftpics = map ((Translate squareSize 0.0) . (\p -> onSquare rd p sq) . buttonPic) leftbuttons
        rightpics = map ((Translate squareSize 0.0) . (\p -> onSquare rd p sq) . buttonPic) rightbuttons
        selectpic = ((Translate squareSize 0.0) . (\p -> onSquare rd p sq) . buttonPic) selected
    in Pictures $ leftpics ++ rightpics ++ [selectpic]
