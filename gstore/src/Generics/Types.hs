{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  , TypeOperators
  , TypeFamilies
  , RankNTypes
  , KindSignatures
 #-}
module Generics.Types where

import Control.Applicative
import Data.Monoid
import Prelude

-- Sum and product types. Just like Either and (,).

infixl 6 :+:
infixl 7 :*:

type a :+: b = Either a b
type a :*: b = (a, b)

-- Helper functions.

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-- Value level fixed point.

fix :: (a -> a) -> a
fix f = f (fix f)

-- Identity annotation.

newtype Id f a = Id { unId :: f a } deriving Show

-- Fixed point combinators and fixed point combinator transformers.

newtype OFix f = OIn (f (OFix f))

newtype FixA (a :: (* -> *) -> (* -> *))
             (f :: (* -> *))
           = In { out :: a f (FixA a f) }

type FixA1 a f = f (FixA a f)
type FixA2 a f = a f (FixA a f)

type Fix  f = FixA Id f

type Fix1 f = f (FixA Id f)
type Fix2 f = Id f (FixA Id f)

-- Constant functor.

newtype K h a = K { unK :: h }
  deriving Monoid

-- Functor composition.

infixl 2 :.:
data (f :.: g) a = C { unC :: f (g a) }
  deriving Functor

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure        = C . pure . pure
  C a <*> C b = C ((<*>) <$> a <*> b)

-- Naturial transformation.

infixl 1 :~>
type f :~> g = forall a. f a -> g a

infixl 2 ::~>
type f ::~> b = forall a. f a -> b

-- Higher order identity annotation.

newtype HId (h  :: (* -> *) -> * -> *)
            (c  :: * -> *)
            (ix :: *)
          = HId { unHId :: h c ix }
  deriving Show

-- Higher order annotated fixed point.

newtype HFixA -- (a  :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *))
              (h  :: (* -> *) -> * -> *)
              (ix :: *)
            = HIn { hout :: h (HFixA h) ix }

-- type HFix h ix = HFixA HId h ix

class HFunctor h where
  hfmap :: (a :~> b) -> h a :~> h b

instance Functor f => HFunctor ((:.:) f) where
  hfmap f = C . fmap f . unC

class HFoldable h where
  hfoldMap :: Monoid m => (a ::~> m) -> h a ::~> m

class (HFunctor h, HFoldable h) => HTraversable h where
  htraverse :: Applicative f => (a :~> f :.: b) -> (h a :~> f :.: h b)

hfold :: HFunctor f => f g :~> g -> HFixA f :~> g
hfold f (HIn u) = f (hfmap (hfold f) u)

-- Fixed point combinator for nested data types with regular nesting.

type family Nest (h :: * -> *) :: * -> *

newtype NFixA a h = NIn { nout :: a h (NFixA a (Nest h)) }

type NFixA1 a h =   h (NFixA a (Nest h))
type NFixA2 a h = a h (NFixA a (Nest h))

