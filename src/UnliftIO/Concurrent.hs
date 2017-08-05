{-# LANGUAGE RankNTypes #-}

-- @TODO: missing forkWithUnmask & forkOnWithUnmask
module UnliftIO.Concurrent
  ( ThreadId
  , myThreadId
  , fork
  , forkFinally
  , killThread
  , throwTo
  , forkOn
  , getNumCapabilities
  , setNumCapabilities
  , threadCapability
  , yield
  , threadDelay
  , threadWaitRead
  , threadWaitWrite
  , C.rtsSupportsBoundThreads
  , forkOS
  , isCurrentThreadBound
  , runInBoundThread
  , runInUnboundThread
  , mkWeakThreadId
  ) where

import           Control.Concurrent ( ThreadId )
import qualified Control.Concurrent as C
import           Control.Exception (SomeException)
import           Control.Monad.IO.Unlift
import           GHC.Weak (Weak)
import           System.Posix.Types (Fd)
import           UnliftIO.Exception


myThreadId :: MonadIO m => m ThreadId
myThreadId = liftIO C.myThreadId
{-# INLINABLE myThreadId #-}

fork :: MonadUnliftIO m => m () -> m ThreadId
fork m = withRunInIO $ \run -> C.forkIO $ run m
{-# INLINABLE fork #-}

forkFinally :: MonadUnliftIO m
            => m a -> (Either SomeException a -> m ()) -> m ThreadId
forkFinally action and_then =
  mask $ \restore -> fork $ try (restore action) >>= and_then
{-# INLINABLE forkFinally #-}

killThread :: MonadIO m => ThreadId -> m ()
killThread = liftIO . C.killThread

forkOn :: MonadUnliftIO m => Int -> m () -> m ThreadId
forkOn n m = withRunInIO $ \run -> C.forkOn n (run m)
{-# INLINABLE forkOn #-}

-- | Generalized version of 'C.getNumCapabilities'.
getNumCapabilities :: MonadIO m => m Int
getNumCapabilities = liftIO C.getNumCapabilities
{-# INLINABLE getNumCapabilities #-}

-- | Generalized version of 'C.setNumCapabilities'.
setNumCapabilities :: MonadIO m => Int -> m ()
setNumCapabilities = liftIO . C.setNumCapabilities
{-# INLINABLE setNumCapabilities #-}

-- | Generalized version of 'C.threadCapability'.
threadCapability :: MonadIO m => ThreadId -> m (Int, Bool)
threadCapability = liftIO . C.threadCapability
{-# INLINABLE threadCapability #-}

-- | Generalized version of 'C.yield'.
yield :: MonadIO m => m ()
yield = liftIO C.yield
{-# INLINABLE yield #-}

-- | Generalized version of 'C.threadDelay'.
threadDelay :: MonadIO m => Int -> m ()
threadDelay = liftIO .  C.threadDelay
{-# INLINABLE threadDelay #-}

-- | Generalized version of 'C.threadWaitRead'.
threadWaitRead :: MonadIO m => Fd -> m ()
threadWaitRead = liftIO . C.threadWaitRead
{-# INLINABLE threadWaitRead #-}

-- | Generalized version of 'C.threadWaitWrite'.
threadWaitWrite :: MonadIO m => Fd -> m ()
threadWaitWrite = liftIO . C.threadWaitWrite
{-# INLINABLE threadWaitWrite #-}

-- | Generalized version of 'C.forkOS'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkOS :: MonadUnliftIO m => m () -> m ThreadId
forkOS m = withRunInIO $ \run -> C.forkOS (run m)
{-# INLINABLE forkOS #-}

-- | Generalized version of 'C.isCurrentThreadBound'.
isCurrentThreadBound :: MonadIO m => m Bool
isCurrentThreadBound = liftIO C.isCurrentThreadBound
{-# INLINABLE isCurrentThreadBound #-}

-- | Generalized version of 'C.runInBoundThread'.
runInBoundThread :: MonadUnliftIO m => m a -> m a
runInBoundThread m = withRunInIO $ \run -> runInBoundThread (run m)
{-# INLINABLE runInBoundThread #-}

-- | Generalized version of 'C.runInUnboundThread'.
runInUnboundThread :: MonadUnliftIO m => m a -> m a
runInUnboundThread m = withRunInIO $ \run -> C.runInUnboundThread (run m)
{-# INLINABLE runInUnboundThread #-}

-- | Generalized versio  of 'C.mkWeakThreadId'.
mkWeakThreadId :: MonadIO m => ThreadId -> m (Weak ThreadId)
mkWeakThreadId = liftIO . C.mkWeakThreadId
{-# INLINABLE mkWeakThreadId #-}
