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
  "(() => {                                                                \
  \  $1.strokeStyle = 'rgba(' + $2 + ',' + $3 + ',' + $4 + ',' + $5 + ')'; \
  \})()"
  js_setStrokeColor
    :: Renderer
    -> Word8  -- ^ red, 0-255
    -> Word8  -- ^ green, 0-255
    -> Word8  -- ^ blue, 0-255
    -> Word8  -- ^ alpha, 0-255
    -> IO ()

foreign import javascript
  "(() => {                   \
  \  $1.beginPath();          \
  \  $1.rect($2, $3, $4, $5); \
  \  $1.stroke();             \
  \})()"
  js_strokeRect
    :: Renderer
    -> Int  -- ^ top-left x
    -> Int  -- ^ top-left y
    -> Int  -- ^ width
    -> Int  -- ^ height
    -> IO ()


type Color = V4 Word8
type Pos = V2 CInt

setStrokeColor
  :: Renderer
  -> Color
  -> IO ()
setStrokeColor renderer (V4 r g b a) = do
  js_setStrokeColor renderer r g b a

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
rectangle renderer (V2 x1 y1) (V2 x2 y2) color = do
  let w = x2 - x1
  let h = y2 - y1
  setStrokeColor renderer color
  js_strokeRect renderer
    (fromIntegral x1)
    (fromIntegral y1)
    (fromIntegral w)
    (fromIntegral h)
