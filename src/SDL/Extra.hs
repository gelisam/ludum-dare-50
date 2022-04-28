module SDL.Extra where

import Control.Exception (bracket, bracket_)
import Data.Text (Text)
import Foreign.C.Types (CInt)
import Linear.Extra
import SDL.Stub (Point(P), V2(..))
import SDL.Font.Stub (Font)
import qualified SDL.Font.Stub as Font
import SDL.Mixer.Stub (Music)
import qualified SDL.Mixer.Stub as Mixer
import SDL.Primitive.Stub (Color, Pos)
import SDL.Video.Stub (Rectangle(..), Surface, Texture, Window, WindowConfig)
import qualified SDL.Video.Stub as Video
import SDL.Video.Renderer.Stub (Renderer, RendererConfig)
import qualified SDL.Video.Renderer.Stub as Renderer


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

withSurfaceTexture
  :: Renderer
  -> Surface
  -> (Texture -> IO a)
  -> IO a
withSurfaceTexture renderer surface
  = bracket
      (Renderer.createTextureFromSurface renderer surface)
      Renderer.destroyTexture

withTextSurface
  :: Font
  -> Color
  -> Text
  -> (Surface -> IO a)
  -> IO a
withTextSurface font color text
  = bracket
      (Font.blended font color text)
      Renderer.freeSurface

withTextTexture
  :: Renderer
  -> Font
  -> Color
  -> Text
  -> (Texture -> IO a)
  -> IO a
withTextTexture renderer font color text body = do
  withTextSurface font color text $ \surface -> do
    withSurfaceTexture renderer surface $ \texture -> do
      body texture

drawCenteredTexture
  :: Renderer
  -> Texture
  -> Pos
  -> IO ()
drawCenteredTexture renderer texture center = do
  textureInfo <- Renderer.queryTexture texture
  let textureSize
        :: V2 CInt
      textureSize
        = V2 (Renderer.textureWidth textureInfo)
             (Renderer.textureHeight textureInfo)
  Renderer.copy
    renderer
    texture
    Nothing  -- full texture
    (Just
    $ Rectangle
        (P (center - half textureSize))
        textureSize
    )
