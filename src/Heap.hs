module Heap where

data Node = 
  Node {
    write :: ByteString -> IO ()
    read  :: 
  }

data Heap =
  Heap {
    alloc :: Integer -> IO Node
    free  :: Node -> IO ()
    read  :: Pointer -> IO Node
  }


{-test = do
  heap <- createHeap "test.db"
  writeList "hallo"

writeList :: String -> IO Node
writeList []     = write 8 (pack32 0)
writeList (x:xs) = do
  n <- writeList xs
  write n 8 (pack32 (1, x, n)-}
