{- | Module     : API
   Maintainer : emi@haskell.fyi
   Description: API for script exporter.

   API for script exporter.
-}
module Data.Cache.Cached (
  cached,
  cachedM,
  cachedForM,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Cache qualified as Cache
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import System.Clock (TimeSpec)

{- | 'cachedFor' but items last forever.

   Uses a HashMap under the hood.
-}
cached :: (Monad m, MonadIO m, Hashable k, Ord k) => (k -> v) -> IO (k -> m v)
cached f = cachedForM Nothing (pure . f)

{- | 'cachedFor' but items last forever.

   Uses a HashMap under the hood.
-}
cachedM :: (Monad m, MonadIO m, Hashable k, Ord k) => (k -> m v) -> IO (k -> m v)
cachedM = cachedForM Nothing

{- | Create a cached version of a function tainting result with MonadIO context.

   Results are cached dependent on the first argument, @'Maybe' 'TimeSpec'@.

   Uses a HashMap under the hood.
-}
cachedForM :: (Monad m, MonadIO m, Hashable k, Ord k) => Maybe TimeSpec -> (k -> m v) -> IO (k -> m v)
cachedForM t f =
  Cache.newCache t <&> \cache k -> do
    res <- liftIO $ Cache.lookup cache k
    case res of
      Nothing -> do
        v <- f k
        liftIO $ Cache.insert cache k v
        pure v
      Just v -> do
        pure v
