{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns, NumericUnderscores, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Tordle (main) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Data.StateVar (($=), get)
import Linear.V2 (V2(..), _x)
import Linear.V4 (V4(..))
import Linear.Vector (unit)
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Video qualified as Video
import SDL.Video.Renderer qualified as Renderer
import SDL.Extra
import Tordle.Assets
import Tordle.Draw
import Tordle.Model


main
  :: IO ()
main = do
  SDL.initializeAll
  withTTF $ do
    Mixer.withAudio Mixer.defaultAudio 1024 $ do
      withWindow "Tordle" Video.defaultWindow $ \window -> do
        windowSize <- get $ Video.windowSize window
        withRenderer window 0 Renderer.defaultRenderer $ \renderer -> do
          withAssets renderer $ \assets -> do
            Renderer.rendererDrawColor renderer $= V4 255 255 255 255
            Renderer.clear renderer
            drawCenteredTexture renderer (assetsTitleTexture assets) (V2 (windowSize^._x `div` 2) 50)
            drawBlock renderer assets Nothing                                      (half windowSize - 3 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block Wild Falling)                  (half windowSize - 2 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'O') Falling)          (half windowSize - 1 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'R') InIncompleteWord) (half windowSize + 0 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'D') NotInWord)        (half windowSize + 1 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'L') WrongSpot)        (half windowSize + 2 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'E') CorrectSpot)      (half windowSize + 3 * unit _x * bLOCK_STRIDE)
            Renderer.present renderer
            Mixer.play (assetsMoveSoundEffect assets)
            threadDelay 3_000_000
