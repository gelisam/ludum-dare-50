{-# LANGUAGE CPP, PatternSynonyms #-}
module SDL.Shim
  ( V2(..)
  , V4(..)
  , Event(..)
  , EventPayload(..)
  , InputMotion(..)
  , KeyModifier(..)
  , KeyboardEventData(..)
  , Keycode
  , pattern KeycodeDown
  , pattern KeycodeEscape
  , pattern KeycodeRight
  , pattern KeycodeLeft
  , pattern KeycodeUp
  , Keysym(..)
  , Point(..)
  , initializeAll
  , time
  , unwrapKeycode
  , waitEventTimeout
  ) where

#ifdef ASTERIUS
import SDL.Asterius
#else
import SDL
#endif
