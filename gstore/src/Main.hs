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

tri :: Storage t (Persistent (Tree Title Movie))
tri = tripletP
  "anch" anchorMan
  "jura" jurassicPark
  "zool" zoolander

main :: IO ()
main =
  do run "../data/test.db" $
       do o <- store nullP
          p <- tri
          reuse o p
          debug

          countP p >>= liftIO . print

          k <- lookupP "anch" p
          liftIO $ print (k :: Maybe Movie)

          k <- lookupP "jura" p
          liftIO $ print (k :: Maybe Movie)

          k <- lookupP "zool" p
          liftIO $ print (k :: Maybe Movie)

-- 
--     dumpAllocationMap
--     dumpHeap
-- 
--     liftIO $ print "---------------------"
--     k <- lookupP "jura" 6
--     liftIO $ print (k :: Maybe Movie)
-- 
--   return ()

