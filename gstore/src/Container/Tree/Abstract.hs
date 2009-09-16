{-# LANGUAGE TemplateHaskell #-}
module Container.Tree.Abstract where

import Control.Applicative
import Data.Binary
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Generics.Representation
import qualified Generics.Regular as R
import Generics.Regular.Binary
import Generics.Regular.TH

-- | Binary tree parametrized by key type, value type and recursive positions.

data Tree k v f =
    Leaf
  | Branch {key :: k, val :: v, leftT :: f, rightT :: f}
  deriving (Eq, Ord, Show, Read)

$(deriveAll ''Tree "PFTree")
type instance R.PF (Tree k v f) = PFTree k v f

instance (Binary k, Binary v, Binary f) => Binary (Tree k v f) where
  put = gput
  get = gget

instance Functor (Tree k v) where
  fmap _ Leaf             = Leaf
  fmap f (Branch k v l r) = Branch k v (f l) (f r)

instance Foldable (Tree k v) where
  foldMap _ Leaf             = mempty
  foldMap f (Branch _ _ l r) = f l `mappend` f r

instance Traversable (Tree k v) where
  traverse _ Leaf             = pure Leaf
  traverse f (Branch k v l r) = Branch k v <$> f l <*> f r

instance (Show k, Show v) => Show (FixT Id (Tree k v)) where
  show k = "[| " ++ show (out k) ++ " |]"

