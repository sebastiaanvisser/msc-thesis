module Generics.Types where

import Control.Applicative
import Data.Monoid
import Prelude hiding (sum)

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

-- Naturial transformation.

type Nat f g = forall a. f a -> g a

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
  hfmap :: (forall a. ix a -> jx a) -> h ix b -> h jx b

class HFoldable h where
  hfoldMap :: Monoid m => (forall a. ix a -> m) -> h ix b -> m

class (HFunctor h, HFoldable h) => HTraversable h where
  htraverse :: Applicative f => (forall a. ix a -> f (jx a)) -> h ix b -> f (h jx b)

-- Fixed point combinator for nested data types with regular nesting.

type family Nest (h :: * -> *) :: * -> *

newtype NFixA a h = NIn { nout :: a h (NFixA a (Nest h)) }

type NFixA1 a h =   h (NFixA a (Nest h))
type NFixA2 a h = a h (NFixA a (Nest h))

