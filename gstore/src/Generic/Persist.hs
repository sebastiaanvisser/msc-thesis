{-# LANGUAGE TypeOperators #-}
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

type PFix f = AnnFix f Persistent
type PFixPtr f = Persistent (PFix f)

-- Persistent producer.

persistentP
  :: (Binary (f (PFixPtr f)), Show (f (PFixPtr f)))
  => ((f (PFixPtr f) -> Storage t (PFixPtr f)) -> Storage t (PFixPtr f))
  -> Storage t (PFixPtr f)
persistentP p = p st

st :: (Binary (f (g (Fix (f :. g)))),
       Show (f (g (Fix (f :. g))))) =>
      f (g (Fix (f :. g))) -> Storage t (Persistent (Fix (f :. g)))
st c =
  do ptr <- store (In (C c)) 
     liftIO $ print (ptr, c)
     return ptr

