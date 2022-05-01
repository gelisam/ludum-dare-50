{-# LANGUAGE CPP #-}
module Loop.Shim where


#ifdef ASTERIUS
import Asterius.Types

foreign import javascript "wrapper" wrapIO
  :: IO () -> IO JSFunction

foreign import javascript "window.requestAnimationFrame($1)" js_requestAnimationFrame
  :: JSFunction -> IO ()
#endif


scheduleNextFrame
  :: IO ()
  -> IO ()
scheduleNextFrame body = do
#ifdef ASTERIUS
  jsBody <- wrapIO body
  js_requestAnimationFrame jsBody
#else
  body
#endif
