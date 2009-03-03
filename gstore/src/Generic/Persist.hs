module Generic.Persist where

import Control.Monad.State
import Data.Binary
import Data.Int
import Generic.Annotate
import Generic.Core
import Heap.Storage
import Prelude hiding (read)
import qualified Data.ByteString.Lazy as B
import Data.Record.Label

type PFix f = AnnFix Persistent f

-- Persistent producer.

persistentP
  :: (Binary (f (Persistent (PFix f))))
  => ((f (Persistent (PFix f)) -> Storage t (Persistent (PFix f))) -> Storage t (f (Persistent (PFix f))))
  -> Storage t (Persistent (PFix f))
persistentP p = let st = store . In . C in p st >>= st

