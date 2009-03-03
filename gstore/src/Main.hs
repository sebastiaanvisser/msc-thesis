module Main where

import Container.Tree
import Control.Monad.State
import Generic.Persist
import Heap.Storage
import MovieDB
import Sample
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as U

-- storeString :: String -> Heap ()
-- storeString s = 
--   let bs = U.fromString s in
--   allocate (fromIntegral $ B.length bs) >>= write bs

-- tri :: Heap Int
-- tri = tripletP
--   "jura" jurassicPark
--   "anch" anchorMan
--   "zool" zoolander

main :: IO ()
main = return ()
--   runHeap "../data/test.db" $ do
--     initHeap
--     tri 
-- 
--     dumpAllocationMap
--     dumpHeap
-- 
--     liftIO $ print "---------------------"
--     k <- lookupP "jura" 6
--     liftIO $ print (k :: Maybe Movie)
-- 
--   return ()

