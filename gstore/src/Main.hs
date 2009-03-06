module Main where

import Prelude hiding (lookup)
import Container.Tree
import Control.Monad.State
import Generic.Persist
import Storage.Storage
import MovieDB
import Sample
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as U

-- storeString :: String -> Heap ()
-- storeString s = 
--   let bs = U.fromString s in
--   allocate (fromIntegral $ B.length bs) >>= write bs

tri :: Storage t (Persistent (Tree Title Movie))
tri = triplet
  "anch" anchorMan
  "jura" jurassicPark
  "zool" zoolander

main :: IO ()
main =
  do run "../data/test.db" $
       do o <- store nullP
          p <- tri
          reuse o p
--           debug

          count p >>= liftIO . print
          liftIO $ putStrLn ""

          k <- lookup "anch" p
          liftIO $ print (k :: Maybe Movie)

          k <- lookup "jura" p
          liftIO $ print (k :: Maybe Movie)

          k <- lookup "zool" p
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

