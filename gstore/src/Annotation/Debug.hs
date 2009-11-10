{-# LANGUAGE UndecidableInstances #-}
module Annotation.Debug where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans
import Data.Binary
import Annotation.Annotation
import Generics.Types
import Prelude hiding ((.), id, read)

newtype Debug f c = Debug { unDebug :: f c }
  deriving Show

instance Binary (f a) => Binary (Debug f a) where
  get = Debug `fmap` get
  put = put . unDebug

instance Show (a f (FixA a f)) => Show (FixA a f) where
  show = show . out

instance (Applicative m, MonadIO m, Show (f (FixA Debug f))) => AnnQ Debug f m where
  query = printer "query" . arr (unDebug . out)

instance (Applicative m, MonadIO m, Show (f (FixA Debug f))) => AnnP Debug f m where
  produce = printer "produce" . arr (In . Debug)

printer :: (MonadIO m, Show b) => String -> Kleisli m b b
printer s = Kleisli (\f -> liftIO (print (s, f)) >> return f)

