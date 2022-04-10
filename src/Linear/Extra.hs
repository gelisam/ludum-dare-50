module Linear.Extra where


-- V2 CInt -> V2 CInt
-- V4 CInt -> V4 CInt
half
  :: (Functor f, Integral a)
  => f a -> f a
half
  = fmap (`div` 2)
