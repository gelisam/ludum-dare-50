{-# LANGUAGE ImportQualifiedPost, NumericUnderscores, OverloadedStrings #-}
module LudumDare (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import Data.StateVar (($=))
import Data.Text (Text)
import Foreign.C.Types (CInt)
import SDL (Point(P), V2(..), V4(..))
import SDL qualified
import SDL.Font (Font)
import SDL.Font qualified as Font
import SDL.Mixer (Music)
import SDL.Mixer qualified as Mixer
import SDL.Primitive (Color)
import SDL.Primitive qualified as Primitive
import SDL.Video (Rectangle(..), Surface, Texture, Window, WindowConfig)
import SDL.Video qualified as Video
import SDL.Video.Renderer (Renderer, RendererConfig)
import SDL.Video.Renderer qualified as Renderer


withLoadable
  :: Mixer.Loadable loadable
  => FilePath
  -> (loadable -> IO a)
  -> IO a
withLoadable filePath
  = bracket
      (Mixer.load filePath)
      (\chunk -> do
         Mixer.halt Mixer.AllChannels  -- docs say not to free while playing
         Mixer.free chunk)

type SoundEffect = Mixer.Chunk

withSoundEffect
  :: FilePath
  -> (SoundEffect -> IO a)
  -> IO a
withSoundEffect
  = withLoadable

withMusic
  :: FilePath
  -> (Music -> IO a)
  -> IO a
withMusic
  = withLoadable

withWindow
  :: Text
  -> WindowConfig
  -> (Window -> IO a)
  -> IO a
withWindow title windowConfig
  = bracket
      (Video.createWindow title windowConfig)
      Video.destroyWindow

withRenderer
  :: Window
  -> CInt  -- ^ what is this?
  -> RendererConfig
  -> (Renderer -> IO a)
  -> IO a
withRenderer window index rendererConfig
  = bracket
      (Video.createRenderer window index rendererConfig)
      Video.destroyRenderer

withTTF
  :: IO a
  -> IO a
withTTF
  = bracket_
      Font.initialize
      Font.quit

withFont
  :: FilePath
  -> Font.PointSize
  -> (Font -> IO a)
  -> IO a
withFont filePath pointSize
  = bracket
      (Font.load filePath pointSize)
      Font.free

withTextSurface
  :: Font
  -> Color
  -> Text
  -> (Surface -> IO a)
  -> IO a
withTextSurface font color text
  = bracket
      (Font.solid font color text)
      Renderer.freeSurface

withSurfaceTexture
  :: Renderer
  -> Surface
  -> (Texture -> IO a)
  -> IO a
withSurfaceTexture renderer surface
  = bracket
      (Renderer.createTextureFromSurface renderer surface)
      Renderer.destroyTexture

main
  :: IO ()
main = do
  SDL.initializeAll
  withTTF $ do
    withFont "assets/kongtext.ttf" 24 $ \font -> do
      Mixer.withAudio Mixer.defaultAudio 1024 $ do
        withSoundEffect "assets/move.wav" $ \moveSoundEffect -> do
          withWindow "Ludum Dare 50" Video.defaultWindow $ \window -> do
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
