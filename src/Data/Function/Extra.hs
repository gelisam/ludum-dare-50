{-# LANGUAGE RankNTypes #-}
module Data.Function.Extra where


anyP
  :: [a -> Bool]
  -> a -> Bool
anyP ps a
  = or $ fmap ($ a) ps

allP
  :: [a -> Bool]
  -> a -> Bool
allP ps a
  = and $ fmap ($ a) ps


newtype With m x = With
  { unWith :: forall a. (x -> m a) -> m a }

withMultiple
  :: Monad m
  => [With m resource]
  -> ([resource] -> m a)
  -> m a
withMultiple [] body = do
  body []
withMultiple (With withResource : withs) body = do
  withResource $ \x -> do
    withMultiple withs $ \xs -> do
      body (x:xs)
