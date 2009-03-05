{-# LANGUAGE TypeOperators, FlexibleContexts #-}
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

type PFix  f = AnnFix f Persistent
type PFixP f = Persistent (PFix f)

-- Persistent producer.

persistentP
  :: Binary (f (PFixP f))
  => ((f (PFixP f) -> Storage t (PFixP f)) -> Storage t (PFixP f))
  -> Storage t (PFixP f)
persistentP p = p produce

produce
  :: Binary (f (PFixP f))
  => f (PFixP f) -> Storage t (PFixP f)
produce = store . In . C

-- Persistent query.

query
  :: Binary (f (PFixP f))
  => PFixP f
  -> Storage t (f (PFixP f))
query p = (unC . out) `liftM` retrieve p



----------

{-produceDebug :: (MonadIO m, Show a) => a -> m a
produceDebug ptr = liftIO (print ptr) >> return ptr

produceId :: Monad m => a -> m a
produceId = return

producePersistentDebugId
  :: (Binary (f (PFixP f))
   , MonadIO ((->) (f (PFixP f)))
   , Show (Storage t (PFixP f)))
  => f (PFixP f)
  -> Storage t (PFixP f) 
producePersistentDebugId = producePersistent >>= produceDebug >>= produceId

persistentDebugIdP
  :: (Binary (f (PFixP f))
   , MonadIO ((->) (f (PFixP f)))
   , Show (Storage t (PFixP f)))
  => ((f (PFixP f) -> Storage t (PFixP f)) -> Storage t (PFixP f))
  -> Storage t (PFixP f)
persistentDebugIdP p = p producePersistentDebugId
-}
