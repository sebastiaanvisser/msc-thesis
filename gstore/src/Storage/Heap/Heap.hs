module Storage.Heap.Heap
  ( run
  , module Storage.Heap.Block
  , HeapR
  , HeapW
  , R.retrieve
  , A.allocate
  , W.store
  )
where

import System.IO
import Storage.Heap.Block
import qualified Storage.Heap.Alloc as A
import qualified Storage.Heap.Read as R
import qualified Storage.Heap.Write as W

type HeapR = R.Heap
type HeapW = W.Heap

run :: FilePath -> W.Heap a -> IO a
run f c =
  withBinaryFile f ReadWriteMode $ \h ->
     (R.run h . A.run h . W.run) c

