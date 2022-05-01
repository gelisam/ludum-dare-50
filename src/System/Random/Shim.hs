{-# LANGUAGE CPP #-}
module System.Random.Shim where

import System.Random (StdGen)
import qualified System.Random as Random


#ifdef ASTERIUS
import Asterius.Types

foreign import javascript "Math.random()" js_random
  :: IO Double
#endif


initStdGen
  :: IO StdGen
initStdGen = do
#ifdef ASTERIUS
  randomDouble <- js_random
  let randomInt = floor (randomDouble * fromIntegral (maxBound :: Int))
  pure $ Random.mkStdGen randomInt
#else
  Random.initStdGen
#endif
