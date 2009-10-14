module System.IO.Lazy where

import Control.Monad.Reader
import System.IO.Unsafe

class Lazy m where
  lazy :: m a -> m a

instance Lazy IO where
  lazy = unsafeInterleaveIO

instance (Monad m, Lazy m) => Lazy (ReaderT r m) where
  lazy c = ask >>= lift . lazy . runReaderT c

