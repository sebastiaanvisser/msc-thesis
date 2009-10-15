module Control.Monad.Lazy where

import Control.Monad.Identity
import Control.Monad.Reader
import System.IO.Unsafe

class Lazy m where
  lazy :: m a -> m a

instance Lazy Identity where
  lazy = return . runIdentity -- Or just `id'.

instance (Monad m, Lazy m) => Lazy (ReaderT r m) where
  lazy c = ask >>= lift . lazy . runReaderT c

instance Lazy IO where
  lazy = unsafeInterleaveIO

class LiftLazy l m where
  liftLazy :: l a -> m a

