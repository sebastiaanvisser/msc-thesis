{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Generics.Arbitrary where

import Control.Applicative
import Control.Monad
import Generic.Core
import Generics.Fixpoints
import Prelude hiding (sum)
import Test.QuickCheck

-- Applicative and alternative instances for the random number generator.

instance Applicative Gen where
  (<*>) = ap
  pure  = return

instance Alternative Gen where
  a <|> b = oneof [a, b]
  empty   = oneof []

-- Generically create arbitrary instances for arbitrary data types.

data ArbParams a = AP {
    rec     :: ArbParams a -> Int -> Gen a
  , divisor :: Int
  }

class GArbitrary f where
  garbitrary'   :: ArbParams a -> Int -> Gen (f a)
  gcoarbitrary' :: (Int -> a -> Gen (g b) -> Gen (g b)) -> Int -> f a -> Gen (g b) -> Gen (g b)

instance GArbitrary Unit where
  garbitrary' _ _ = pure Unit
  gcoarbitrary' _ _ _ = id

instance GArbitrary Id where
  garbitrary' p@(AP r d) t = Id <$> r p (t `div` d)
  gcoarbitrary' r o (Id a) = r o a

instance Arbitrary a => GArbitrary (K a) where
  garbitrary' _ _ = K <$> arbitrary
  gcoarbitrary' _ _ (K a) = coarbitrary a

instance (GFixpoints f, GFixpoints g,
          GArbitrary f, GArbitrary g) => GArbitrary (Sum f g) where
  garbitrary' p s = frequency [r fpl Inl, r fpr Inr]
    where r a b = (fr a, b <$> garbitrary' p {divisor = sum a} s)
          (Node fpl fpr) = gfixpoints' (undefined :: Sum f g a)
          fr = foldTree (f s) (+)
          f 0 0 = 1 -- if our size (s) is zero, we only give non-recursive constructors a chance
          f 0 _ = 0
          f _ x = x + 1
  gcoarbitrary' r o (Inl f) = gcoarbitrary' r  o f
  gcoarbitrary' r o (Inr f) = gcoarbitrary' r (o + sum fpl) f
    where (Node fpl _) = gfixpoints' (undefined :: Sum f g a)

instance (GArbitrary f, GArbitrary g) => GArbitrary (Prod f g) where
  garbitrary' p s = Prod <$> garbitrary' p s <*> garbitrary' p s
  gcoarbitrary' r _ (Prod f g) = gcoarbitrary' r 0 f . gcoarbitrary' r 0 g

instance GArbitrary f => GArbitrary (Con f) where
  garbitrary' p s = Con "" <$> garbitrary' p s
  gcoarbitrary' f o (Con _ c) = variant o . gcoarbitrary' f 0 c

instance GArbitrary f => GArbitrary (F f) where
  garbitrary' p s = F <$> garbitrary' p s
  gcoarbitrary' f o (F c) = gcoarbitrary' f o c

-- The first undefined gets filled in with garbitraryFix, the second in the instance for Sum.
garbitrary :: (GArbitrary (PF a), PFView a) => Int -> Gen a
garbitrary s = garbitraryHelper (AP undefined 1) s

garbitraryHelper :: (PFView a, GArbitrary (PF a)) => ArbParams a -> Int -> Gen a
garbitraryHelper p s = to <$> garbitrary' p {rec = garbitraryHelper} s

