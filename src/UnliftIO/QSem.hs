module UnliftIO.QSem
  ( QSem
  , newQSem
  , waitQSem
  , signalQSem
  ) where

import           Control.Concurrent.QSem (QSem)
import qualified Control.Concurrent.QSem as QSem
import           Control.Monad.IO.Unlift

newQSem :: MonadIO m => Int -> m QSem
newQSem = liftIO . QSem.newQSem

waitQSem :: MonadIO m => QSem -> m ()
waitQSem = liftIO . QSem.waitQSem

signalQSem :: MonadIO m => QSem -> m ()
signalQSem = liftIO . QSem.signalQSem
