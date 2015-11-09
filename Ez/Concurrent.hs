-- |
-- Module      :  Ez.Concurrent
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for running code concurrently.

module Ez.Concurrent (parMapSeqFinalizeIO) where

  import Control.Concurrent.Async

  -- | Runs a mapping IO function in parallel on items in a list, and
  --   a finalizing IO function in sequence on each of the results of the mapping
  --   function in the order they appear in the original list.
  parMapSeqFinalizeIO :: (a -> IO b) -- ^ mapping function, applied in parallel on the items in the list
                      -> Int -- ^ maximum number of parallel executions of the mapping function
                      -> (b -> IO ()) -- ^ finalize function, applied in sequence on the mapping results
                      -> [a] -- ^ list of items to process
                      -> IO ()
  parMapSeqFinalizeIO m f c xs = parMapSeqFinalizeIOHelper m f c xs []

  -- | Helper for parMapSeqFinalizeIO.
  parMapSeqFinalizeIOHelper :: (a -> IO b) -- ^ mapping function, applied in parallel on the items in the list
                            -> Int -- ^ maximum number of parallel executions of the mapping function
                            -> (b -> IO ()) -- ^ finalize function, applied in sequence on the mapping results
                            -> [a] -- ^ list of items to process
                            -> [Async b] -- ^ list of currently running async mapping functions
                            -> IO ()
  parMapSeqFinalizeIOHelper _ _ _ [] [] = return ()
  parMapSeqFinalizeIOHelper m c f [] (a:as) = do
      finalizeHelper f a
      parMapSeqFinalizeIOHelper m c f [] as
  parMapSeqFinalizeIOHelper m c f (x:xs) [] = do
      newA <- async (m x)
      parMapSeqFinalizeIOHelper m c f xs [newA]
  parMapSeqFinalizeIOHelper m c f xAll@(x:xs) aAll@(a:as)
    | length aAll >= c = do
        finalizeHelper f a
        parMapSeqFinalizeIOHelper m c f xAll as
    | otherwise = do
        newA <- async (m x)
        parMapSeqFinalizeIOHelper m c f xs (aAll ++ [newA])

  -- | Helper for parMapSeqFinalizeIO that waits on the result of an async
  --   mapping and then applies the finalize function on it.
  finalizeHelper :: (b -> IO ()) -- ^ finalize function
                 -> Async b -- ^ async mapping result which is waited for
                 -> IO ()
  finalizeHelper f ax = do
      x <- wait ax
      f x
