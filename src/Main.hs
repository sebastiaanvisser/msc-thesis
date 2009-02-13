module Main where

import Control.Monad.State
import Heap.FileHeap
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U

storeString :: String -> Heap ()
storeString s = 
  let bs = U.fromString s in
  allocate (B.length bs) >>= write bs

main :: IO ()
main = do
  runHeap "../data/test.db" $ do
    get >>= liftIO . print 
    dumpHeap

    storeString "hallo!"
    storeString "zomaar viel spaß jö!"
    storeString "1234567890123"
    storeString "haa"
    storeString "ykykykyky"

    pointer 72 >>= free
    storeString "1234"
    pointer 99 >>= free

    get >>= liftIO . print 
    dumpHeap
  return ()


