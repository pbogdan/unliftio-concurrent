module UnliftIO.QSemN
  ( QSemN
  , newQSemN
  , waitQSemN
  , signalQSemN
  ) where

import           Control.Concurrent.QSemN (QSemN)
import qualified Control.Concurrent.QSemN as QSemN
import           Control.Monad.IO.Unlift

newQSemN :: MonadUnliftIO m => Int -> m QSemN
newQSemN = liftIO . QSemN.newQSemN

waitQSemN :: MonadUnliftIO m => QSemN -> Int -> m ()
waitQSemN sem = liftIO . QSemN.waitQSemN sem

signalQSemN :: MonadUnliftIO m => QSemN -> Int -> m ()
signalQSemN sem = liftIO . QSemN.signalQSemN sem
