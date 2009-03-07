{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Container.Tree
import Control.Monad.State
import Generic.Representation
import MovieDB
import Prelude hiding (lookup)
import Sample
import Storage.Storage

sep a b =
  do liftIO $ putStrLn ("\n----[  " ++ a ++ "  ]-----------------\n")
     r <- b
     liftIO $ putStrLn "- - - - - - -"
     debug
     return r

main :: IO ()
main =
  run "../data/test.db" $
    do o <- store nullP

       
       p <-     (sep "empty")       empty
            >>= (sep "ins jura") . insert "jura" jurassicpark
            >>= (sep "ins anch") . insert "anch" anchorman
            >>= (sep "ins zool") . insert "zool" zoolander
       reuse o (unC p)

       count p >>= \(c :: Int) -> liftIO (print c)

       
       sep "anch" (lookup "anch" p >>= \(k :: Maybe Movie) -> liftIO (print k))
       sep "jura" (lookup "jura" p >>= \(k :: Maybe Movie) -> liftIO (print k))
       sep "zool" (lookup "zool" p >>= \(k :: Maybe Movie) -> liftIO (print k))

       return ()

