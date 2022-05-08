module SDL.Font.Asterius
  ( Font
  , PointSize
  , blended
  , free
  , initialize
  , load
  , quit
  ) where

import Data.Text
import SDL.Primitive.Asterius (Color)
import SDL.Video.Asterius.Types


data Font
  = FontStub
  deriving Show

type PointSize = Int


blended
  :: Font
  -> Color
  -> Text
  -> IO Surface
blended _ _ _ = do
  putStrLn "Font.blended: stub"
  pure SurfaceStub

free
  :: Font
  -> IO ()
free _ = do
  putStrLn "Font.free: stub"

initialize
  :: IO ()
initialize = do
  putStrLn "Font.initialize: stub"

load
  :: FilePath
  -> PointSize
  -> IO Font
load _ _ = do
  putStrLn "Font.load: stub"
  pure FontStub

quit
  :: IO ()
quit = do
  putStrLn "Font.quit: stub"
