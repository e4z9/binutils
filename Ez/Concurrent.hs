module Ez.Concurrent (parMapSeqFinalizeIO) where

  import Control.Concurrent.Async

  -- mapFun -> capabilities -> finalizeFun -> list -> nothing
  parMapSeqFinalizeIO :: (a -> IO b) -> Int -> (b -> IO ()) -> [a] -> IO ()
  parMapSeqFinalizeIO m f c xs = parMapSeqFinalizeIOHelper m f c xs []

  -- mapFun -> finalizeFun -> mapQueue -> finalizeQueue -> nothing
  parMapSeqFinalizeIOHelper :: (a -> IO b) -> Int -> (b -> IO ()) -> [a] -> [Async b] -> IO ()
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

  finalizeHelper :: (b -> IO ()) -> Async b -> IO ()
  finalizeHelper f ax = do
      x <- wait ax
      f x
