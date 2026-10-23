{-# LANGUAGE ScopedTypeVariables #-}

-- | A fixed-size worker pool on plain forkIO threads.
module Pool (parMapPool) where

import           Control.Concurrent (forkIO, modifyMVar, newEmptyMVar, newMVar,
                                     putMVar, takeMVar)
import           Control.Exception (SomeException, evaluate, throwIO, try)
import           Control.Monad (forM, replicateM_)

-- | Run the action over every list element using at most k worker
-- threads (never more than there are jobs). Each job's own result MVar
-- travels with it through the queue, so results come back in input
-- order. Results are forced to WHNF inside the worker — paired with
-- strict result fields that keeps the real work off the caller's
-- thread. An exception in any job is re-thrown in the caller.
parMapPool :: forall a b. Int -> (a -> IO b) -> [a] -> IO [b]
parMapPool k f xs = do
  slots <- forM xs (const newEmptyMVar)
  queue <- newMVar (zip slots xs)
  let pop = modifyMVar queue $ \q -> pure $ case q of
        []     -> ([], Nothing)
        j : js -> (js, Just j)
      worker = do
        mjob <- pop
        case mjob of
          Nothing -> pure ()
          Just (slot, x) -> do
            r <- try (f x >>= evaluate) :: IO (Either SomeException b)
            putMVar slot r
            worker
  replicateM_ (max 1 (min k (length xs))) (forkIO worker)
  mapM (\s -> takeMVar s >>= either throwIO pure) slots
