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
     b

main :: IO ()
main =
  run "../data/test.db" $
    do o <- store nullP

       
       p <-     (sep "empty")       empty
            >>= (sep "ins anch") . insert "anch" anchorman
            >>= (sep "ins zool") . insert "zool" zoolander
            >>= (sep "ins jura") . insert "jura" zoolander
       reuse o (unC p)
--        p' <- 
--        p'' <- insert "jura" jurassicpark p'
--        p''' <- insert "zool" zoolander p''
-- 
--        reuse o p'''

--        debug

--        count p >>= \(c :: Int) -> liftIO (print c)
--        sep "lookup"
--        lookup "anch" p >>= \(k :: Maybe Movie) -> liftIO (print k)
--        lookup "jura" p >>= \(k :: Maybe Movie) -> liftIO (print k)
--        lookup "zool" p >>= \(k :: Maybe Movie) -> liftIO (print k)

       return ()

