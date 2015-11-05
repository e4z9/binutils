-- |
-- Module      :  Ez.Concurrent
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for running code concurrently.

module Ez.Concurrent (mapReduceIO) where

  import Control.Concurrent.Async

  -- | Runs a mapping IO function in parallel on items in a list, and
  --   a reducing IO function in sequence on each of the results of the mapping
  --   function in the order they appear in the original list.
  mapReduceIO :: (a -> IO b) -- ^ mapping function, applied in parallel on the items in the list
              -> Int -- ^ maximum number of parallel executions of the mapping function
              -> (b -> c -> IO c) -- ^ reduce function, applied in sequence on the mapping results,
                                  --   gets the next element and previous aggregated value, returns next aggregated value
              -> c -- ^ initial aggregated value for reduce function
              -> [a] -- ^ list of items to process
              -> IO c -- ^ returns final aggregated value of the reduce function
  mapReduceIO m c f agg xs = mapReduceIOHelper m c f agg xs []

  -- | Helper for mapReduceIO.
  mapReduceIOHelper :: (a -> IO b) -- ^ mapping function, applied in parallel on the items in the list
                    -> Int -- ^ maximum number of parallel executions of the mapping function
                    -> (b -> c -> IO c) -- ^ reduce function, applied in sequence on the mapping results,
                                        --   gets the next element and previous aggregated value, returns next aggregated value
                    -> c -- ^ current aggregated value for reduce function
                    -> [a] -- ^ list of items to process
                    -> [Async b] -- ^ list of currently running async mapping functions
                    -> IO c
  mapReduceIOHelper _ _ _ agg [] [] = return agg
  mapReduceIOHelper m c f agg [] (a:as) = do
      newAgg <- reduceHelper f agg a
      mapReduceIOHelper m c f newAgg [] as
  mapReduceIOHelper m c f agg (x:xs) [] = do
      newA <- async (m x)
      mapReduceIOHelper m c f agg xs [newA]
  mapReduceIOHelper m c f agg xAll@(x:xs) aAll@(a:as)
    | length aAll >= c = do
        newAgg <- reduceHelper f agg a
        mapReduceIOHelper m c f newAgg xAll as
    | otherwise = do
        newA <- async (m x)
        mapReduceIOHelper m c f agg xs (aAll ++ [newA])

  -- | Helper for mapReduceIO that waits on the result of an async
  --   mapping and then applies the reduce function on it.
  reduceHelper :: (b -> c -> IO c) -- ^ reduce function
               -> c -- ^ current aggregated value for reduce function
               -> Async b -- ^ async mapping result which is waited for
               -> IO c
  reduceHelper f agg ax = do
      x <- wait ax
      f x agg
