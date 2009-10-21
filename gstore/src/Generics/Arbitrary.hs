{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Generics.Arbitrary where

import Control.Applicative
import Control.Monad
import Generics.Regular.Base
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

data ArbParams a = AP
  { rec     :: ArbParams a -> Int -> Gen a
  , divisor :: Int
  }

class GArbitrary f where
  garbitrary'   :: ArbParams a -> Int -> Gen (f a)
  gcoarbitrary' :: (Int -> a -> Gen (g b) -> Gen (g b)) -> Int -> f a -> Gen (g b) -> Gen (g b)

instance GArbitrary I where
  garbitrary' p@(AP r d) t = I <$> r p (t `div` d)
  gcoarbitrary' r o (I a) = r o a

instance GArbitrary U where
  garbitrary' _ _ = pure U
  gcoarbitrary' _ _ _ = id

instance Arbitrary a => GArbitrary (K a) where
  garbitrary' _ _ = K <$> arbitrary
  gcoarbitrary' _ _ (K a) = coarbitrary a

instance (GFixp f, GFixp g, GArbitrary f, GArbitrary g) => GArbitrary (f :+: g) where
  garbitrary' p s = frequency [r fpl L, r fpr R]
    where r a b = (fr a, b <$> garbitrary' p {divisor = sum a} s)
          (Node fpl fpr) = fixp (undefined :: (f :+: g) a)
          fr = foldTree (f s) (+)
          f 0 0 = 1 -- if our size (s) is zero, we only give non-recursive constructors a chance
          f 0 _ = 0
          f _ x = x + 1
  gcoarbitrary' r o (L f) = gcoarbitrary' r  o f
  gcoarbitrary' r o (R f) = gcoarbitrary' r (o + sum fpl) f
    where (Node fpl _) = fixp (undefined :: (f :+: g) a)

instance (GArbitrary f, GArbitrary g) => GArbitrary (f :*: g) where
  garbitrary' p s = (:*:) <$> garbitrary' p s <*> garbitrary' p s
  gcoarbitrary' r _ (f :*: g) = gcoarbitrary' r 0 f . gcoarbitrary' r 0 g

instance GArbitrary f => GArbitrary (C c f) where
  garbitrary' p c = C <$> garbitrary' p c
  gcoarbitrary' f o (C c) = variant o . gcoarbitrary' f 0 c

instance GArbitrary f => GArbitrary (S s f) where
  garbitrary' p s = S <$> garbitrary' p s
  gcoarbitrary' f o (S s) = variant o . gcoarbitrary' f 0 s

-- The first undefined gets filled in with garbitraryFix, the second in the instance for sums.
garbitrary :: (Regular a, GArbitrary (PF a)) => Int -> Gen a
garbitrary s = garbitraryHelper (AP undefined 1) s

garbitraryHelper :: (Regular a, GArbitrary (PF a)) => ArbParams a -> Int -> Gen a
garbitraryHelper p s = to <$> garbitrary' p {rec = garbitraryHelper} s

