{-# LANGUAGE
    TypeOperators
  , FlexibleContexts
  , MultiParamTypeClasses
  , FlexibleInstances
  , TypeFamilies
 #-}
module Aspect.Debug where

import Control.Monad.State hiding (get, put)
import Data.Binary
import Generics.Aspect
import Generics.Regular
import Prelude hiding (read)

newtype Debug a = Debug { unDebug :: a }
  deriving Show

instance Binary a => Binary (I a) where
  get = I `fmap` get
  put = put . unI

instance Binary a => Binary (Debug a) where
  get = Debug `fmap` get
  put = put . unDebug

instance (MonadIO m, Show f) => Aspect Debug f m where
  produce f = liftIO (print ("produce:", f)) >> return (Debug f)
  query   f = liftIO (print ("query:",   f)) >> return (unDebug f)

  modify    = ( \f -> liftIO (print ("modify_p:", f)) >> return (Debug f)
              , \f -> liftIO (print ("modify_q:", f)) >> return (unDebug f))

instance Unwrap f => Unwrap (Debug f) where
  type UW (Debug f) = UW f
  unwrap = unwrap . unDebug

