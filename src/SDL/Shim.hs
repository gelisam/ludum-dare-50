{-# LANGUAGE DeriveGeneric #-}
module SDL.Shim
  ( V2(..)
  , V4(..)
  , module SDL.Shim
  ) where

import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Linear.V2
import Linear.V4


data Event = Event
  { eventPayload
      :: EventPayload
  } deriving (Show)

data EventPayload
  = KeyboardEvent KeyboardEventData
  | QuitEvent
  deriving (Generic, Show)

data InputMotion
  = Released
  | Pressed
  deriving (Eq, Show)

data KeyModifier =  KeyModifier
  { keyModifierLeftShift
      :: Bool
  , keyModifierRightShift
      :: Bool
  }
  deriving Show

data KeyboardEventData = KeyboardEventData
  { keyboardEventKeyMotion
      :: InputMotion
  , keyboardEventKeysym
      :: Keysym
  }
  deriving (Generic, Show)

data Keycode
  = KeycodeDown
  | KeycodeEscape
  | KeycodeLeft
  | KeycodeR
  | KeycodeRight
  | KeycodeTab
  | KeycodeUp
  deriving (Eq, Show)

data Keysym = Keysym
  { keysymKeycode
      :: Keycode
  , keysymModifier
      :: KeyModifier
  }
  deriving Show

newtype Point f a
  = P (f a)
  deriving Show

initializeAll
  :: IO ()
initializeAll = do
  putStrLn "SDL.initializeAll: stub"

time
  :: IO Double
time = do
  putStrLn "SDL.time: stub"
  pure 0

unwrapKeycode
  :: Keycode
  -> Int
unwrapKeycode KeycodeDown
  = 1073741905
unwrapKeycode KeycodeEscape
  = 27
unwrapKeycode KeycodeLeft
  = 1073741904
unwrapKeycode KeycodeR
  = 114
unwrapKeycode KeycodeRight
  = 1073741903
unwrapKeycode KeycodeTab
  = 9
unwrapKeycode KeycodeUp
  = 1073741906

waitEventTimeout
  :: CInt
  -> IO (Maybe Event)
waitEventTimeout _ = do
  putStrLn "SDL.waitEventTimeout: stub"
  pure $ Just $ Event QuitEvent
