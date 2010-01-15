{-# LANGUAGE
    DeriveFunctor
  , EmptyDataDecls
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

-- infixl 6 :+:
-- infixl 7 :*:

-- type a :+: b = Either a b
-- type a :*: b = (a, b)

-- Helper functions.

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-- Value level fixed point.

fix :: (a -> a) -> a
fix f = f (fix f)

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

-- Peano numbers.

data Zero
data Succ c

type family Prev a
type instance Prev (Succ c) = c

type family   Fmap (f :: * -> *) a
type instance Fmap f (a, b)       = (a, f b)
type instance Fmap f (Maybe a)    = Maybe (f a)
type instance Fmap f (Either a b) = Either a (f b)

-- Continue counting.

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

-- Indexed functions.

data (:->) a b ix = F { unF :: a ix -> b ix }

(#) :: (a :-> b) ix -> a ix -> b ix
(#) (F x) y = x y

-- Constant functor.

newtype K h a = K { unK :: h }
  deriving Monoid

castK :: K h a -> K h b
castK = K . unK

-- Indexed pair.

infixl 6 :+:
infixl 7 :*:

data (f :+: g) ix = L { unL :: f ix } | R { unR :: g ix }
data (f :*: g) ix = (:*:) { hfst :: f ix, hsnd :: g ix }

type family Fst a :: *
type instance Fst (a, b) = a
type instance Fst ((a :*: b) ix) = a ix

type family Snd a :: *
type instance Snd (a, b) = b
type instance Snd ((a :*: b) ix) = b ix

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

newtype HFix -- (a  :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *))
              (h  :: (* -> *) -> * -> *)
              (ix :: *)
            = HIn { hout :: h (HFix h) ix }

-- type HFix h ix = HFix HId h ix

class HFunctor h where
  hfmap :: (a :~> b) -> h a :~> h b

instance Functor f => HFunctor ((:.:) f) where
  hfmap f = C . fmap f . unC

class HFoldable h where
  hfoldMap :: Monoid m => (a ::~> m) -> h a ::~> m

class (HFunctor h, HFoldable h) => HTraversable h where
  htraverse :: Applicative f => (a :~> f :.: b) -> (h a :~> f :.: h b)

class HApplicative h where
  hpure :: a :~> h a
  (<#>) :: (a :~> b) -> h a :~> f b

type HAlg f g = f g :~> g

hfold :: HFunctor f => HAlg f a -> HFix f :~> a
hfold f (HIn u) = f (hfmap (hfold f) u)

foldm :: (HFoldable h, Monoid m) => (forall b. h b ::~> m) -> HFix h c -> m
foldm f = hfoldMap (\x -> f (hout x) `mappend` foldm f x) . hout

type HPara f g = f (HFix f :*: g) :~> g

hpara :: HFunctor f => HPara f a -> HFix f :~> a
hpara f (HIn u) = f (hfmap (\x -> x :*: hpara f x) u)

newtype Ran g h a = Ran { unRan :: (a -> g) -> h }

-- gfold :: HAlg f g -> (HFix f :.: g) :~> h
-- gfold = undefined

-- gfoldk :: HFunctor h => (h (Ran g f) :~> Ran g f) -> (a -> g) -> FixA h a -> f
-- gfoldk alg g m = unRan (hfold alg m) g


-- Fixed point combinator for nested data types with regular nesting.

type family Nest (h :: * -> *) :: * -> *

newtype NFixA a h = NIn { nout :: a h (NFixA a (Nest h)) }

type NFixA1 a h =   h (NFixA a (Nest h))
type NFixA2 a h = a h (NFixA a (Nest h))

