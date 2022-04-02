{-# LANGUAGE ImportQualifiedPost, NumericUnderscores, OverloadedStrings #-}
module LudumDare (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Data.StateVar (get, ($=))
import Data.Text (Text)
import Foreign.C.Types (CInt)
import SDL (V2(..), V4(..))
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Primitive qualified as Primitive
import SDL.Video qualified as Video
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

withChunk
  :: FilePath
  -> (Mixer.Chunk -> IO a)
  -> IO a
withChunk
  = withLoadable

withMusic
  :: FilePath
  -> (Mixer.Music -> IO a)
  -> IO a
withMusic
  = withLoadable

withWindow
  :: Text
  -> Video.WindowConfig
  -> (Video.Window -> IO a)
  -> IO a
withWindow title windowConfig
  = bracket
      (Video.createWindow title windowConfig)
      Video.destroyWindow

withRenderer
  :: Video.Window
  -> CInt  -- ^ what is this?
  -> Renderer.RendererConfig
  -> (Renderer.Renderer -> IO a)
  -> IO a
withRenderer window index rendererConfig
  = bracket
      (Video.createRenderer window index rendererConfig)
      Video.destroyRenderer

main
  :: IO ()
main = do
  SDL.initializeAll
  Mixer.withAudio Mixer.defaultAudio 1024 $ do
    withChunk "assets/move.wav" $ \moveChunk -> do
      withWindow "Ludum Dare 50" Video.defaultWindow $ \window -> do
        withRenderer window 0 Renderer.defaultRenderer $ \renderer -> do
          Renderer.rendererDrawColor renderer $= V4 0 0 0 255
          Renderer.clear renderer
          Primitive.fillRectangle renderer (V2 100 100) (V2 200 200) (SDL.V4 0 0 255 255)
          Renderer.present renderer
          Mixer.play moveChunk
          threadDelay 1_000_000
