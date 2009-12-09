{-# LANGUAGE
    TemplateHaskell
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Container.FingerTree.Abstract where

import Control.Applicative
import Data.Monoid
import Generics.Types

-- Peano numbers.

data Zero
data Succ c

-- Continue counting.

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

-- Nodes in a finger tree either represent a part of the spine or a part of the
-- value fingers.

data Spine c
data Value c

data Finger a (f :: * -> *) :: * -> * where
  Empty  :: Finger a f (Spine Zero)
  Deep   :: f (Value (Succ c)) -> f (Value (Succ c)) -> f (Spine c) -> Finger a f (Spine (Succ c))
  Leaf   :: Int -> Finger a f (Value One)
  Branch :: f (Value c) -> f (Value c) -> Finger a f (Value (Succ c))

empty_ :: HFixA (Finger a) (Spine Zero)
empty_ = HIn Empty

deep :: HFixA (Finger a) (Value (Succ c)) -> HFixA (Finger a) (Value (Succ c)) -> HFixA (Finger a) (Spine c) -> HFixA (Finger a) (Spine (Succ c))
deep a b c = HIn (Deep a b c)

leaf :: Int -> HFixA (Finger a) (Value One)
leaf i = HIn (Leaf i)

node :: HFixA (Finger a) (Value c) -> HFixA (Finger a) (Value c) -> HFixA (Finger a) (Value (Succ c))
node a b = HIn (Branch a b)

myFinger :: HFixA (Finger Int) (Spine Three)
myFinger = deep e e . deep c d . deep a b $ empty_
  where
  a = leaf 1
  b = leaf 2
  c = node (leaf 3) (leaf 4)
  d = node (leaf 5) (leaf 6)
  e = node c d

instance HFunctor (Finger a) where
  hfmap _ Empty        = Empty
  hfmap f (Deep a b c) = Deep (f a) (f b) (f c)
  hfmap _ (Leaf i)     = Leaf i
  hfmap f (Branch a b) = Branch (f a) (f b)

instance HFoldable (Finger a) where
  hfoldMap _ Empty        = mempty
  hfoldMap f (Deep a b c) = f a `mappend` f b `mappend` f c
  hfoldMap _ (Leaf _)     = mempty
  hfoldMap f (Branch a b) = f a `mappend` f b

instance HTraversable (Finger a) where
  htraverse _ Empty        = pure Empty
  htraverse f (Deep a b c) = Deep <$> f a <*> f b <*> f c
  htraverse _ (Leaf i)     = pure (Leaf i)
  htraverse f (Branch a b) = Branch <$> f a <*> f b

