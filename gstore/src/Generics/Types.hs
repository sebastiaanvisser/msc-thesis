module Generics.Types where

import Prelude hiding (sum)

-- Identity annotation.

newtype Id f a = Id { unId :: f a } deriving Show

-- Fixed point combinators and fixed point combinator transformers.

newtype FixA (a :: (* -> *) -> (* -> *))
             (f :: (* -> *))
           = In { out :: a f (FixA a f) }

type FixA1 a f = f (FixA a f)
type FixA2 a f = a f (FixA a f)

type Fix  f = FixA Id f
type Fix1 f = f (FixA Id f)
type Fix2 f = Id f (FixA Id f)

-- Value level fixed point.

fix :: (a -> a) -> a
fix f = f (fix f)

-- Sum and product types. Just like Either and (,).

infixl 6 :+:
infixl 7 :*:

type a :+: b = Either a b
type a :*: b = (a, b)

-- Helper functions.

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

newtype HId (h  :: (* -> *) -> (* -> *))
            (a  :: (* -> *))
            (ix :: *)
          = HId { unHId :: h a ix }
  deriving Show

newtype HFixA (a  :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
              (h  :: (* -> *) -> * -> *)
              (ix :: *)
            = HIn { hout :: a h (HFixA a h) ix }

type HFix h ix = HFixA HId h ix

-- Fixed point combinator for nested data types with regular nesting.

type family Nest (h :: * -> *) :: * -> *

newtype NFix h = NIn { nout :: h (NFix (Nest h)) }

