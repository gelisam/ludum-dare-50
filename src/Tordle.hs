{-# LANGUAGE ImportQualifiedPost, NumericUnderscores, OverloadedStrings #-}
module Tordle (main) where

import Control.Concurrent (threadDelay)
import Data.StateVar (($=))
import SDL (Point(P), V2(..), V4(..))
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Primitive qualified as Primitive
import SDL.Video (Rectangle(..))
import SDL.Video qualified as Video
import SDL.Video.Renderer qualified as Renderer
import SDL.Extra


main
  :: IO ()
main = do
  SDL.initializeAll
  withTTF $ do
    withFont "assets/kongtext.ttf" 24 $ \font -> do
      Mixer.withAudio Mixer.defaultAudio 1024 $ do
        withSoundEffect "assets/move.wav" $ \moveSoundEffect -> do
          withWindow "Tordle" Video.defaultWindow $ \window -> do
            withRenderer window 0 Renderer.defaultRenderer $ \renderer -> do
              withTextSurface font (V4 0 255 255 255) "Tordle" $ \surface -> do
                withSurfaceTexture renderer surface $ \texture -> do
                  info <- Renderer.queryTexture texture
                  print info
                  Renderer.rendererDrawColor renderer $= V4 0 0 0 255
                  Renderer.clear renderer
                  Primitive.fillRectangle renderer (V2 100 100) (V2 200 200) (V4 0 0 255 255)
                  Renderer.copy
                    renderer
                    texture
                    Nothing  -- full texture
                    ( Just
                    $ Rectangle
                        (P $ V2 100 100)
                        (V2 (100 + Renderer.textureWidth info)
                            (100 + Renderer.textureHeight info))
                    )
                  Renderer.present renderer
                  Mixer.play moveSoundEffect
                  threadDelay 1_000_000
