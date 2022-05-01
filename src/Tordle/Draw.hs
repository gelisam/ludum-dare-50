{-# LANGUAGE NamedFieldPuns, OverloadedLabels, RecordWildCards, ScopedTypeVariables #-}
module Tordle.Draw where

import Control.Lens
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.StateVar (($=), get)
import Foreign.C.Types (CInt)
import Linear.Extra
import Linear.V2 (V2(..), _x, _y)
import Linear.V4 (V4(..))
import SDL.Primitive.Shim (Color, Pos)
import qualified SDL.Primitive.Shim as Primitive
import SDL.Video.Shim (Window)
import qualified SDL.Video.Shim as Video
import SDL.Video.Renderer.Shim (Renderer, Texture)
import qualified SDL.Video.Renderer.Shim as Renderer
import SDL.Extra
import Tordle.Assets
import Tordle.Colors
import Tordle.Guess
import Tordle.Model


bLOCK_SIZE
  :: Num a => a
bLOCK_SIZE
  = 40

bLOCK_GAP
  :: Num a => a
bLOCK_GAP
  = 4

bLOCK_STRIDE
  :: Num a => a
bLOCK_STRIDE
  = bLOCK_SIZE + bLOCK_GAP

oUTLINE_SIZE
  :: Num a => a
oUTLINE_SIZE
  = 2


drawMaybeSolution
  :: Renderer
  -> Assets
  -> Maybe String
  -> Pos
  -> IO ()
drawMaybeSolution renderer assets maybeSolution center = do
  for_ maybeSolution $ \solution -> do
    let w :: CInt
        w = fromIntegral $ length solution
        topLeft
          :: Pos
        topLeft
          = center - half ((V2 w 2 - 1) * bLOCK_STRIDE)
    drawMaybeHelpText renderer assets (Just HelpSolution) (V2 (center^._x) (topLeft^._y))
    for_ (zip [0..] solution) $ \(i, letter) -> do
      let block = Block (Letter letter) CorrectSpot 0 0
      drawBlock renderer assets block (topLeft + V2 i 1 * bLOCK_STRIDE)

drawMaybeHelpText
  :: Renderer
  -> Assets
  -> Maybe HelpText
  -> Pos
  -> IO ()
drawMaybeHelpText renderer (Assets {assetsHelpTextTextures}) maybeHelpText pos = do
  for_ maybeHelpText $ \helpText -> do
    let texture
          :: Texture
        texture
          = assetsHelpTextTextures ^?! ix helpText
    drawCenteredTexture renderer texture pos

drawSolidBlock
  :: Renderer
  -> Pos  -- center
  -> Color
  -> IO ()
drawSolidBlock renderer pos color = do
  Primitive.fillRectangle renderer (pos - half bLOCK_SIZE) (pos + half bLOCK_SIZE) color

drawOutlineBlock
  :: Renderer
  -> Pos  -- center
  -> Color
  -> IO ()
drawOutlineBlock renderer pos color = do
  for_ [0..oUTLINE_SIZE-1] $ \(i :: CInt) -> do
    Primitive.rectangle
      renderer
      (pos - half bLOCK_SIZE + fromIntegral i)
      (pos + half bLOCK_SIZE - fromIntegral i + 1)
      color

drawLetter
  :: Renderer
  -> Map Char Texture
  -> Char
  -> Pos
  -> IO ()
drawLetter renderer letterTextures char pos = do
  drawCenteredTexture renderer (letterTextures ! char) pos

drawLabel
  :: Renderer
  -> Map Char Texture
  -> Label
  -> Pos
  -> IO ()
drawLabel renderer letterTextures label pos = do
  case label of
    Letter char -> do
      drawLetter renderer letterTextures char pos
    Wild -> do
      pure ()

drawGridBlock
  :: Renderer
  -> Pos
  -> IO ()
drawGridBlock renderer pos = do
  drawOutlineBlock renderer pos lightGray

drawBlock
  :: Renderer
  -> Assets
  -> Block
  -> Pos
  -> IO ()
drawBlock renderer (Assets {assetsBlackLetterTextures, assetsWhiteLetterTextures}) (Block {..}) pos = do
  let pos' = pos + fmap round (bLOCK_STRIDE * blockOffset)
  case blockStatus of
    Falling -> do
      drawSolidBlock renderer pos' blue
      drawLabel renderer assetsWhiteLetterTextures blockLabel pos'
    InIncompleteWord -> do
      drawSolidBlock renderer pos' white
      drawOutlineBlock renderer pos' gray
      drawLabel renderer assetsBlackLetterTextures blockLabel pos'
    NotInWord -> do
      drawSolidBlock renderer pos' gray
      drawLabel renderer assetsWhiteLetterTextures blockLabel pos'
    WrongSpot -> do
      drawSolidBlock renderer pos' yellow
      drawLabel renderer assetsWhiteLetterTextures blockLabel pos'
    CorrectSpot -> do
      drawSolidBlock renderer pos' green
      drawLabel renderer assetsWhiteLetterTextures blockLabel pos'

drawGrid
  :: Renderer
  -> Pos
  -> IO ()
drawGrid renderer center = do
  let topLeft
        :: Pos
      topLeft
        = center - half ((pHANTOM_BOARD_SIZE - 1) * bLOCK_STRIDE)
  for_ [0..fULL_BOARD_SIZE^._y - 1] $ \j -> do
    for_ [0..fULL_BOARD_SIZE^._x -1] $ \i -> do
      let ij
            :: V2 CInt
          ij
            = V2 i j
      when (inMainBoard ij) $ do
        drawGridBlock renderer (topLeft + ij * bLOCK_STRIDE)

drawBoardLayer
  :: Renderer
  -> Assets
  -> Board
  -> Pos
  -> IO ()
drawBoardLayer renderer assets board center = do
  let topLeft
        :: Pos
      topLeft
        = center - half ((pHANTOM_BOARD_SIZE - 1) * bLOCK_STRIDE)
  for_ [0..fULL_BOARD_SIZE^._y - 1] $ \j -> do
    for_ [0..fULL_BOARD_SIZE^._x -1] $ \i -> do
      let ij
            :: V2 CInt
          ij
            = V2 i j
      let maybeBlock
            :: Maybe Block
          maybeBlock
            = Map.lookup ij board
      for_ maybeBlock $ \block -> do
        drawBlock renderer assets block (topLeft + ij * bLOCK_STRIDE)

drawBoard
  :: Renderer
  -> Assets
  -> Board
  -> Pos
  -> IO ()
drawBoard renderer assets board center = do
  drawGrid renderer center
  let maybeMinZIndex = minimumOf (each . #blockZIndex) board
  let maybeMaxZIndex = maximumOf (each . #blockZIndex) board
  for_ maybeMinZIndex $ \minZIndex -> do
    for_ maybeMaxZIndex $ \maxZIndex -> do
      for_ [minZIndex..maxZIndex] $ \zIndex -> do
        let board' = Map.filter ((== zIndex) . blockZIndex) board
        drawBoardLayer renderer assets board' center

drawAlphabetColoring
  :: Renderer
  -> Assets
  -> AlphabetColoring
  -> Pos
  -> IO ()
drawAlphabetColoring renderer assets alphabedColoring center = do
  let rows
        :: [String]
      rows
        = [ "ABCDEFGHI"
          , "JKLMNOPQR"
          , "STUVWXYZ"
          ]
  for_ (zip [0..] rows) $ \(j, row) -> do
    let w :: CInt
        w = fromIntegral $ length row
        topLeft
          :: Pos
        topLeft
          = center - half ((V2 w 3 - 1) * bLOCK_STRIDE)
    for_ (zip [0..] row) $ \(i, letter) -> do
      let maybeGuessResult = Map.lookup letter alphabedColoring
      let block = Block (Letter letter) (maybeGuessStatus maybeGuessResult) 0 0
      drawBlock renderer assets block (topLeft + V2 i j * bLOCK_STRIDE)

presentWorld
  :: Window
  -> Renderer
  -> Assets
  -> World
  -> IO ()
presentWorld window renderer assets (World {..}) = do
  windowSize <- get $ Video.windowSize window
  Renderer.rendererDrawColor renderer $= V4 255 255 255 255
  Renderer.clear renderer
  let bigTextTexture = case worldStatus of
        Playing
          -> assetsTitleTexture assets
        GameOver
          -> assetsGameOverTexture assets
        Win
          -> assetsWinTexture assets
  drawCenteredTexture renderer bigTextTexture
    (V2 (1 * windowSize^._x `div` 3 - 10) (1 * windowSize^._y `div` 3))
  drawMaybeSolution renderer assets worldMaybeSolution
    (V2 (1 * windowSize^._x `div` 3 - 10) (1 * windowSize^._y `div` 3 + 70))
  drawMaybeHelpText renderer assets worldMaybeHelpText
    (V2 (1 * windowSize^._x `div` 3 - 10) (1 * windowSize^._y `div` 2))
  drawAlphabetColoring renderer assets worldAlphabetColoring
    (V2 (1 * windowSize^._x `div` 3 - 10) (2 * windowSize^._y `div` 3))
  let board'
        = case worldStatus of
            Playing
              -> renderPiece worldCurrentPiece <> worldBoard
            _ -> worldBoard
  drawBoard renderer assets board'
    (V2 (3 * windowSize^._x `div` 4 + 10) (windowSize^._y `div` 2))
  Renderer.present renderer
