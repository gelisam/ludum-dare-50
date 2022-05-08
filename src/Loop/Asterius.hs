module Loop.Asterius where

import Asterius.Types


foreign import javascript "wrapper" wrapIO
  :: IO () -> IO JSFunction

foreign import javascript "window.requestAnimationFrame($1)" js_requestAnimationFrame
  :: JSFunction -> IO ()


scheduleNextFrame
  :: IO ()
  -> IO ()
scheduleNextFrame body = do
  jsBody <- wrapIO body
  js_requestAnimationFrame jsBody
