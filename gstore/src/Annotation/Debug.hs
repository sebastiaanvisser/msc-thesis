{-# LANGUAGE UndecidableInstances #-}
module Annotation.Debug where

import Control.Arrow
import Control.Category
import Control.Monad.Trans
import Data.Binary
import Annotation.Annotation
import Generics.Representation
import Prelude hiding ((.), id, read)

newtype Debug f c = Debug { unDebug :: f c }
  deriving Show

instance Binary (f a) => Binary (Debug f a) where
  get = Debug `fmap` get
  put = put . unDebug

instance Show (a f (FixT a f)) => Show (FixT a f) where
  show = show . out

instance (MonadIO m, Show (f (FixT Debug f))) => Annotation Debug f m where
  query   = printer "query"   . arr unDebug
  produce = printer "produce" . arr Debug

printer :: (MonadIO m, Show b) => String -> Kleisli m b b
printer s = Kleisli (\f -> liftIO (print (s, f)) >> return f)

