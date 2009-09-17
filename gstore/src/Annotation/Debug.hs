module Annotation.Debug where

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

-- TODO: fix show
instance MonadIO m => Annotation Debug f m where
  query   = printer "query"   . arr unDebug
  produce = printer "produce" . arr Debug

printer :: MonadIO m => String -> Kleisli m b b
printer s = Kleisli (\f -> liftIO (print (s)) >> return f)

