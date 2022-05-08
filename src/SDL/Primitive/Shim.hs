{-# LANGUAGE CPP #-}
module SDL.Primitive.Shim
  ( Color
  , Pos
  , fillRectangle
  , rectangle
  ) where

#ifdef ASTERIUS
import SDL.Primitive.Asterius
#else
import SDL.Primitive
#endif
