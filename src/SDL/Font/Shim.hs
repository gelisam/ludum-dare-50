{-# LANGUAGE CPP #-}
module SDL.Font.Shim
  ( Font
  , PointSize
  , blended
  , free
  , initialize
  , load
  , quit
  ) where

#ifdef ASTERIUS
import SDL.Font.Asterius
#else
import SDL.Font
#endif
