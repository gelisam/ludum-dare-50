{-# LANGUAGE ImportQualifiedPost, NumericUnderscores, OverloadedStrings #-}
module Tordle (main) where

import Control.Concurrent (threadDelay)
import Data.StateVar (($=))
import SDL (V2(..), V4(..))
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Primitive qualified as Primitive
import SDL.Video qualified as Video
import SDL.Video.Renderer qualified as Renderer
import SDL.Extra


main
  :: IO ()
main = do
  SDL.initializeAll
  withTTF $ do
    withFont "assets/clear-sans.regular.ttf" 24 $ \font -> do
      Mixer.withAudio Mixer.defaultAudio 1024 $ do
        withSoundEffect "assets/move.wav" $ \moveSoundEffect -> do
          withWindow "Tordle" Video.defaultWindow $ \window -> do
            withRenderer window 0 Renderer.defaultRenderer $ \renderer -> do
              withTextSurface font (V4 0 255 255 255) "Tordle" $ \surface -> do
                withSurfaceTexture renderer surface $ \texture -> do
                  Renderer.rendererDrawColor renderer $= V4 0 0 0 255
                  Renderer.clear renderer
                  Primitive.fillRectangle renderer (V2 100 100) (V2 200 200) (V4 0 0 255 255)
                  drawCenteredTexture renderer texture (V2 150 150)
                  Renderer.present renderer
                  Mixer.play moveSoundEffect
                  threadDelay 1_000_000
