module Heap.Heap
  ( run
  , module Heap.Block
  , HeapR
  , HeapW
  , R.retrieve
  , A.allocate
  , W.store
  , W.unsafeReuse
  )
where

import System.IO
import Heap.Block
import qualified Heap.Alloc as A
import qualified Heap.Read as R
import qualified Heap.Write as W

type HeapR = R.Heap
type HeapW = W.Heap

run :: FilePath -> W.Heap a -> IO a
run f c =
  withBinaryFile f ReadWriteMode $ \h ->
     (R.run h . A.run h . W.run) c

