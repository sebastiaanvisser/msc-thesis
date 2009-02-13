module Generic.Persist where

import Prelude hiding (read)
import Heap.FileHeap
import Container.Tree
import Generic.Annotate

type PTree a b = FTree a b Pointer

persistentQ f a = monadicQ proc (f a) 
  where proc c = do
                    k <- read c
                    let m = undefined k
                    return (m :: PTree Char String)


-- readF o = do
--   bs <- read o
