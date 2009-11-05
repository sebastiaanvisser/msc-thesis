module Generics.Types where

import Prelude hiding (sum)

-- Identity annotation.

newtype Id f a = Id { unId :: f a } deriving Show

-- Fixed point combinators and fixed point combinator transformers.

newtype FixA (a :: (* -> *) -> (* -> *)) f = In { out :: a f (FixA a f) }
type FixA1 a f = f (FixA a f)
type FixA2 a f = a f (FixA a f)

type Fix f = FixA Id f
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

