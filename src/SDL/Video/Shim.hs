{-# LANGUAGE CPP #-}
module SDL.Video.Shim
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

#ifdef ASTERIUS
import SDL.Video.Asterius
#else
import SDL.Video
#endif
