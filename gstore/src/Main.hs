{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Container.Tree
import Control.Monad.State
import MovieDB
import Prelude hiding (lookup)
import Sample
import Storage.Storage

main :: IO ()
main =
  run "../data/test.db" $
    do o <- store nullP
       p <- tri
       reuse o p
--           debug

       count p >>= \(c :: Int) -> liftIO (print c)

       lookup "anch" p >>= \(k :: Maybe Movie) -> liftIO (print k)
       lookup "jura" p >>= \(k :: Maybe Movie) -> liftIO (print k)
       lookup "zool" p >>= \(k :: Maybe Movie) -> liftIO (print k)


