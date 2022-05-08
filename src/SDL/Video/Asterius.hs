module SDL.Video.Asterius
  ( Rectangle(..)
  , Surface
  , Texture
  , Window
  , WindowConfig(..)
  , createWindow
  , destroyWindow
  , createRenderer
  , defaultWindow
  , destroyRenderer
  , windowSize
  ) where

import Asterius.Types
import Data.StateVar
import Data.Text (Text)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL.Video.Asterius.Types


foreign import javascript
  "(() => {                                           \
  \  const div = document.createElement('div');       \
  \  div.style.width = '' + $1 + 'px';                \
  \  div.style.height = '' + $2 + 'px';               \
  \  div.style.margin = 'auto';                       \
  \  const canvas = document.createElement('canvas'); \
  \  canvas.id = 'canvas';                            \
  \  canvas.width = $1;                               \
  \  canvas.height = $2;                              \
  \  div.appendChild(canvas);                         \
  \  document.body.appendChild(div);                  \
  \  return canvas;                                   \
  \})()"
  js_createCanvas :: Int -> Int -> IO JSVal


createWindow
  :: Text  -- title
  -> WindowConfig
  -> IO Window
createWindow title windowConfig = do
  let V2 w h = windowInitialSize windowConfig
  Window <$> js_createCanvas (fromIntegral w) (fromIntegral h)

destroyWindow
  :: Window
  -> IO ()
destroyWindow _ = do
  -- intentionally not removing the canvas because the top-level 'bracket' ends
  -- too soon.
  pure ()

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
