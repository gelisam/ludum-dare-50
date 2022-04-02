{-# LANGUAGE ImportQualifiedPost #-}
module SDL.Extra where

import Control.Exception (bracket, bracket_)
import Data.Text (Text)
import Foreign.C.Types (CInt)
import SDL.Font (Font)
import SDL.Font qualified as Font
import SDL.Mixer (Music)
import SDL.Mixer qualified as Mixer
import SDL.Primitive (Color)
import SDL.Video (Surface, Texture, Window, WindowConfig)
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
