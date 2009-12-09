{-# LANGUAGE UndecidableInstances #-}
module Annotation.Debug where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans
import Data.Binary
import Annotation.NAnnotation
import Generics.Types
import Prelude hiding ((.), id, read)

newtype Debug h c = Debug { unDebug :: h c }
  deriving Show

instance Binary (h a) => Binary (Debug h a) where
  get = Debug `fmap` get
  put = put . unDebug

instance Show (NFixA2 a h) => Show (NFixA a h) where
  show = show . nout

instance (Applicative m, MonadIO m, Show (NFixA1 Debug h)) => AnnQ Debug h m where
  query = printer "query" . arr (unDebug . nout)

instance (Applicative m, MonadIO m, Show (NFixA1 Debug h)) => AnnP Debug h m where
  produce = printer "produce" . arr (NIn . Debug)

printer :: (MonadIO m, Show b) => String -> Kleisli m b b
printer s = Kleisli (\h -> liftIO (print (s, h)) >> return h)

