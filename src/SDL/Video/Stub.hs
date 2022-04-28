module SDL.Video.Stub
  ( Rectangle(..)
  , Surface
  , Texture
  , Window
  , WindowConfig(..)
  , module SDL.Video.Stub
  ) where

import Data.StateVar
import Data.Text (Text)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL.Video.Stub.Types


createWindow
  :: Text  -- title
  -> WindowConfig
  -> IO Window
createWindow _ _ = do
  putStrLn "Video.createWindow: stub"
  pure WindowStub

destroyWindow
  :: Window
  -> IO ()
destroyWindow _ = do
  putStrLn "Video.destroyWindow: stub"

createRenderer
  :: Window
  -> CInt
  -> RendererConfig
  -> IO Renderer
createRenderer _ _ _ = do
  putStrLn "Video.createRenderer: stub"
  pure RendererStub

defaultWindow
  :: WindowConfig
defaultWindow
  = WindowConfig (V2 800 600)

destroyRenderer
  :: Renderer
  -> IO ()
destroyRenderer _ = do
  putStrLn "Video.destroyRenderer: stub"

windowSize
  :: Window
  -> StateVar (V2 CInt)
windowSize _ = StateVar getter setter
  where
    getter
      :: IO (V2 CInt)
    getter = do
      putStrLn "Video.windowSize / get: stub"
      pure 0

    setter
      :: V2 CInt
      -> IO ()
    setter _ = do
      putStrLn "Video.windowSize / set: stub"
