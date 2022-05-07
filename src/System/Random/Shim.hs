{-# LANGUAGE CPP #-}
module System.Random.Shim
  ( initStdGen
  ) where

#ifdef ASTERIUS

import Asterius.Types
import System.Random (StdGen)
import qualified System.Random as Random

foreign import javascript "Math.random()" js_random
  :: IO Double


initStdGen
  :: IO StdGen
initStdGen = do
  randomDouble <- js_random
  let randomInt = floor (randomDouble * fromIntegral (maxBound :: Int))
  pure $ Random.mkStdGen randomInt


#else
import System.Random
#endif
