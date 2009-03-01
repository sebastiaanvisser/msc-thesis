module Generic.Persist where

import Control.Monad.State
import Data.Binary
import Data.Int
import Generic.Annotate
import Generic.Core
import Heap.FileHeap
import Prelude hiding (read)
import qualified Data.ByteString.Lazy as B
import Data.Record.Label

-- type PTree a b = FTree a b Pointer

-- Persistent producer.

persistentP :: Binary a => ((a -> Heap Int) -> Heap a) -> Heap Int
persistentP p = p serializeWrite >>= serializeWrite

serializeWrite :: Binary a => a -> Heap Int
serializeWrite a = 
  do let bs = encode a
     b <- allocate (fromIntegral (B.length bs))
     write bs b
     return (pointer b)




-- readF o = do
--   bs <- read o
