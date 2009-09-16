module Aspect.Debug where

import Control.Arrow
import Control.Category
import Control.Monad.Trans
import Data.Binary
import Generics.Annotation
import Prelude hiding ((.), id, read)

newtype Debug f c = Debug { unDebug :: f c }
  deriving Show

instance Binary (f a) => Binary (Debug f a) where
  get = Debug `fmap` get
  put = put . unDebug

instance (MonadIO m, Show (f c)) => Annotation Debug f c m where
  produce = printer "produce:" . arr Debug
  query   = printer "query:"   . arr unDebug

printer :: (MonadIO m, Show b) => String -> Kleisli m b b
printer s = Kleisli (\f -> liftIO (print (s, f)) >> return f)

