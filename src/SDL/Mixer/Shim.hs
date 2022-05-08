{-# LANGUAGE CPP, PatternSynonyms #-}
module SDL.Mixer.Shim
  ( Audio(..)
  , Channel
  , pattern AllChannels
  , Chunk(..)
  , ChunkSize
  , Loadable(..)
  , Music(..)
  , defaultAudio
  , halt
  , play
  , withAudio
  ) where

#ifdef ASTERIUS
import SDL.Mixer.Asterius
#else
import SDL.Mixer
#endif
