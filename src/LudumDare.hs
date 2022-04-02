{-# LANGUAGE ImportQualifiedPost, NumericUnderscores #-}
module LudumDare (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import SDL qualified
import SDL.Mixer qualified as Mixer


withLoadable
  :: Mixer.Loadable loadable
  => FilePath
  -> (loadable -> IO a)
  -> IO a
withLoadable filePath
  = bracket
      (Mixer.load filePath)
      (\chunk -> do
         Mixer.halt Mixer.AllChannels  -- docs say not to free while playing
         Mixer.free chunk)

withChunk
  :: FilePath
  -> (Mixer.Chunk -> IO a)
  -> IO a
withChunk
  = withLoadable

withMusic
  :: FilePath
  -> (Mixer.Music -> IO a)
  -> IO a
withMusic
  = withLoadable

main
  :: IO ()
main = do
  SDL.initializeAll
  Mixer.withAudio Mixer.defaultAudio 1024 $ do
    withChunk "assets/move.wav" $ \moveChunk -> do
      Mixer.play moveChunk
      threadDelay 1_000_000
