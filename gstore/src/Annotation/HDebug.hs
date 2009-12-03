{-# LANGUAGE UndecidableInstances #-}
module Annotation.HDebug where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans
import Data.Binary
import Annotation.HAnnotation
import Generics.Types
import Prelude hiding ((.), id, read)

newtype HDebug (h  :: (* -> *) -> (* -> *))
               (a  :: (* -> *))
               (ix :: *)
             = HDebug { unHDebug :: h a ix }
  deriving Show

instance Binary (h a ix) => Binary (HDebug h a ix) where
  get = HDebug `fmap` get
  put = put . unHDebug

instance Show (a h (HFixA a h) ix) => Show (HFixA a h ix) where
  show = show . hout

instance (Applicative m, MonadIO m, Show (h (HFixA HDebug h) ix)) => HAnnQ HDebug h ix m where
  query = printer "query" . arr (unHDebug . hout)

instance (Applicative m, MonadIO m, Show (HFixA HDebug h ix)) => HAnnP HDebug h ix m where
  produce = printer "produce" . arr (HIn . HDebug)

printer :: (MonadIO m, Show b) => String -> Kleisli m b b
printer s = Kleisli (\f -> liftIO (print (s, f)) >> return f)

