module SDL.Mixer.Shim where


data Audio
  = AudioStub
  deriving Show

data Channel
  = AllChannels
  deriving Show

data Chunk
  = ChunkStub
  deriving Show

type ChunkSize = Int

class Loadable a where
  load
    :: FilePath -> IO a
  free
    :: a -> IO ()

instance Loadable Chunk where
  load _ = do
    putStrLn "Mixer.load @Chunk: stub"
    pure ChunkStub
  free _ = do
    putStrLn "Mixer.free @Chunk: stub"

instance Loadable Music where
  load _ = do
    putStrLn "Mixer.load @Music: stub"
    pure MusicStub
  free _ = do
    putStrLn "Mixer.free @Music: stub"

data Music
  = MusicStub
  deriving Show

defaultAudio
  :: Audio
defaultAudio
  = AudioStub

halt
  :: Channel
  -> IO ()
halt AllChannels = do
  putStrLn "Mixer.halt: stub"

play
  :: Chunk
  -> IO () 
play _ = do
  putStrLn "Mixer.play: stub"

withAudio
  :: Audio
  -> ChunkSize
  -> IO a
  -> IO a 
withAudio _ _ body = do
  body
