{-# LANGUAGE
    TypeOperators
  , FlexibleContexts
  , MultiParamTypeClasses
  , FlexibleInstances
 #-}
module Aspect.Debug where

import Control.Monad.State hiding (get, put)
import Data.Binary
import Generic.Aspect
import Generic.Representation
import Prelude hiding (read)

newtype Debug a = D { unD :: a }
  deriving Show

instance Binary a => Binary (Id a) where
  get = Id `fmap` get
  put = put . unId

instance Binary a => Binary (Debug a) where
  get = D `fmap` get
  put = put . unD

instance (MonadIO m, Show f) => Aspect Debug f m where
  produce f = liftIO (print ("produce:", f)) >> return (D f)
  query   f = liftIO (print ("query:",   f)) >> return (unD f)

  modify    = ( \f -> liftIO (print ("modify_p:", f)) >> return (D f)
              , \f -> liftIO (print ("modify_q:", f)) >> return (unD f))
