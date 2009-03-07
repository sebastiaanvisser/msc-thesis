{-# LANGUAGE
    TypeOperators
  , FlexibleContexts
  , MultiParamTypeClasses
  , FlexibleInstances
 #-}
module Aspect.Debug where

import Control.Applicative
import Control.Monad.State hiding (get, put)
import Data.Binary
import Aspect.Aspect
import Generic.Annotate
import Generic.Representation
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

instance (MonadIO m, Show f) => Aspect Debug f m where
  produce f = liftIO (print f) >> return (D f)
