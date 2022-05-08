module Loop where

scheduleNextFrame
  :: IO ()
  -> IO ()
scheduleNextFrame body = do
  body
