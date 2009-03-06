{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Aspect.Persistent where

import Control.Applicative
import Control.Monad.State
import Data.Binary
import Generic.Annotate
import Generic.Core
import Prelude hiding (read)
import Storage.Storage

type PFix  f = AnnFix f Persistent
type PFixP f = Persistent (PFix f)

-- Persistent producer.

persistentP
  :: Binary (f (PFixP f))
  => Producer f (PFixP f) (Storage t)
  -> Storage t (PFixP f)
persistentP p = p produce

produce
  :: Binary (f (PFixP f))
  => f (PFixP f) -> Storage t (PFixP f)
produce = store . In . C

-- Persistent query.

persistentQ
  :: (Binary (f (PFixP f)))
  => Query f (PFixP f) (Storage t) c
  -> PFixP f -> Storage t c
persistentQ q f = query f >>= worker
  where worker = q (\p -> query p >>= worker)

query
  :: Binary (f (PFixP f))
  => PFixP f
  -> Storage t (f (PFixP f))
query p = (unC . out) `liftM` retrieve p

-- Persistent modifier.

persistentM
  :: Binary (f (PFixP f))
  => Modifier f (PFixP f) (Storage t) c
  -> PFixP f -> Storage t c
persistentM m f = query f >>= worker
  where worker = m produce (\p -> (query p <* delete p) >>= worker)

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
