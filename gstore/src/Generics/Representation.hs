module Generics.Representation where

-- Identity annotation.

newtype Id f a = Id { unId :: f a }
  deriving Show

-- Fixed point combinators and combinator transformers.

type Fix f = FixT Id f
type Fix1 f = f (FixT Id f)

newtype FixT (a :: (* -> *) -> (* -> *)) f = In { out :: a f (FixT a f) }
type FixT1 a f = a f (FixT a f)

-- Value level fixed point.

fix :: (a -> a) -> a
fix f = f (fix f)

