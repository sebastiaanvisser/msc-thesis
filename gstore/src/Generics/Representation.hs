module Generics.Representation where

import Data.Foldable
-- import Generics.Regular.Base
-- import Data.Binary

-- deriving instance Show f           => Show (I f)
-- deriving instance Show (f (Fix f)) => Show (Fix f)

-- instance Binary (f (Fix f)) => Binary (Fix f) where
--   put = put . out
--   get = In `fmap` get

-- Function composition at the type level.
-- Compose :: (* -> *) -> (* -> *) -> * -> *
infixr :.:
newtype (f :.: g) a = O { unO :: f (g a) }
  deriving Show

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f = O . fmap (fmap f) . unO

instance (Foldable f, Foldable g) => Foldable (f :.: g) where
  foldMap f = foldMap (foldMap f) . unO

-- Identity aspect.

newtype Id f a = Id { unId :: f a }

newtype FixT (a :: (* -> *) -> (* -> *)) f = In { out :: a f (FixT a f) }
type Fix f = FixT Id f

-- type AnnFixF a                           f = (a f) (Fix (a f))
-- type MuF f = Id f (FixT Id f)

