{-# LANGUAGE CPP #-}
module System.Random.Shim
  ( initStdGen
  ) where

#ifdef ASTERIUS
import System.Random.Asterius
#else
import System.Random
#endif
