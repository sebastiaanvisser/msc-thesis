{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Aspect.Debug where

import Control.Applicative
import Control.Monad.State hiding (get, put)
import Data.Binary
import Generic.Annotate
import Generic.Core
import Prelude hiding (read)
import Storage.Storage

newtype Debug a = D { unD :: a }
  deriving Show

instance Binary a => Binary (Id a) where
  get = Id `fmap` get
  put = put . unId

instance Binary a => Binary (Debug a) where
  get = D `fmap` get
  put = put . unD

-- Persistent producer.

produce_P :: Binary f => f -> Storage t (Pointer f)
produce_P f = store f

produce_D :: (Show f, MonadIO m) => f -> m (Debug f)
produce_D f = liftIO (print f) >> return (D f)

produce_I :: Monad m => f -> m (Id f)
produce_I f = return (Id f)

combine
  :: Monad m
  => (a -> m b)
  -> (b -> m (f (g c)))
  -> a -> m ((:.) f g c)
combine a b = a >=> liftM C . b





-------------------------------------------

type MyAspects = Pointer :. Debug :. Id
type MyFix  f = Fix (f :. MyAspects)
type MyFixM f = MyAspects (MyFix f)

myP
  :: (Show (f (MyFixM f)), Binary (f (MyFixM f)))
  => Producer f (MyFixM f) (Storage t)
  -> Storage t (MyFixM f)
myP p = p myaspects

myaspects
  :: (Show (f (MyFixM f)), Binary (f (MyFixM f)))
  => f (MyFixM f)
  -> Storage t (MyFixM f)
myaspects = (produce_I `combine` produce_D `combine` produce_P) . In . C


-------------------------------------------

data FList a f = Nil | Cons a f
  deriving Show

instance Binary (FList a f) where
  put = undefined
  get = undefined

type MyList a = MyFix (FList a)

