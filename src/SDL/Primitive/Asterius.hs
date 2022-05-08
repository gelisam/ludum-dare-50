module SDL.Primitive.Asterius
  ( Color
  , Pos
  , fillRectangle
  , rectangle
  ) where

import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import SDL.Video.Asterius.Types


foreign import javascript
  "(() => {                   \
  \  $1.beginPath();          \
  \  $1.rect($2, $3, $4, $5); \
  \  $1.stroke();             \
  \})()"
  js_drawRect
    :: Renderer
    -> Int  -- ^ top-left x
    -> Int  -- ^ top-left y
    -> Int  -- ^ width
    -> Int  -- ^ height
    -> IO ()


type Color = V4 Word8
type Pos = V2 CInt

fillRectangle
  :: Renderer
  -> Pos
  -> Pos
  -> Color
  -> IO ()
fillRectangle _ _ _ _ = do
  putStrLn "Primitive.fillRectangle: stub"

rectangle
  :: Renderer
  -> Pos
  -> Pos
  -> Color
  -> IO ()
rectangle renderer (V2 x1 y1) (V2 x2 y2) _ = do
  let w = x2 - x1
  let h = y2 - y1
  js_drawRect renderer
    (fromIntegral x1)
    (fromIntegral y1)
    (fromIntegral w)
    (fromIntegral h)
