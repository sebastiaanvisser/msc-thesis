module System.IO.Lazy where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.IO.Unsafe

newtype Lazy a = Lazy { runLazy :: IO a }
  deriving Functor

instance Monad Lazy where
  return = Lazy . return
  a >>= f = Lazy ((runLazy a) >>= runLazy . f)

instance MonadIO Lazy where
  liftIO = Lazy . unsafeInterleaveIO

instance Applicative Lazy where
  pure = return
  (<*>) = ap
