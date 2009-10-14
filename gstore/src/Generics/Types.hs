module Generics.Types where

-- Identity annotation.

newtype Id f a = Id { unId :: f a }
  deriving Show

-- Fixed point combinators and fixed point combinator transformers.

type Fix f = FixT Id f
type Fix1 f = f (FixT Id f)

newtype FixT (a :: (* -> *) -> (* -> *)) f = In { out :: a f (FixT a f) }
type FixT1 a f = a f (FixT a f)

-- Value level fixed point.

fix :: (a -> a) -> a
fix f = f (fix f)

-- Sum and product types. Just like Either and (,).

infixl 6 :+:
infixl 7 :*:

data a :+: b = L a | R b
type a :*: b = (a, b)

