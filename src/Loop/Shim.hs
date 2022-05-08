{-# LANGUAGE CPP #-}
module Loop.Shim
  ( scheduleNextFrame
  ) where

#ifdef ASTERIUS
import Loop.Asterius
#else
import Loop
#endif
