module Generics.Morph where

import Control.Arrow
import Control.Category
import Control.Monad hiding (sequence)
import Control.Monad.Identity
import Control.Monad.Lazy
import Data.Traversable
import Annotation.Annotation
import Generics.Types
import Prelude hiding ((.), id, sequence)

-- todo: cata/ana in terms of para/apop.

type Phi' a f c = f c :*: f (FixT a f) -> c
type Phi    f c = forall a. Phi' a f c

-- how lazy is this?
paraT :: (Traversable f, Lazy m, AnnQ a f m) => Phi' a f c -> a f (FixT a f) -> m c
paraT phi f = 
  do g <- runKleisli query f
     fc <- sequence (fmap (lazy . paraT phi . out) g)
     return (phi (fc, g))

-- | Apomorphism for functors with annotated fixpoint in an monadic context.
-- todo: generalize to true apo by skipping the endo part.

type Seed s a f = s :*: f (FixT a f)
type Stop   a f = FixT a f :+: f (FixT a f)
type Next s a f = s :*: FixT a f :+: s :*: f (FixT a f)
type Psi' s a f = Seed s a f -> f (Stop a f :+: Next s a f)
type Psi  s   f = forall a. Psi' s a f

apoT
  :: (Traversable f, AnnM a f m)
  => Psi' s a f -> s -> FixT1 a f -> m (FixT1 a f)
apoT psi s = runKleisli $ modify (Kleisli (sequence . fmap rec . psi . (,) s))
  where
  rec e =
    case e of
      L (L x)      -> return x
      L (R x)      -> In `liftM` runKleisli produce x
      R (L (t, x)) -> In `liftM` apoT psi t (out x)
      R (R (t, x)) -> In `liftM` (runKleisli produce x >>= apoT psi t)

-- | Apomorphism for functors with fixpoint in an applicative context.

apoA
  :: (Traversable f, Monad m)
  => Psi' s Id f -> s -> Fix f -> m (Fix f)
apoA psi s = return . In <=< apoT psi s . out

-- | Plain apomorphism for functors with fixpoint.

apo :: Traversable f => Psi' s Id f -> s -> Fix f -> Fix f
apo psi s = runIdentity . apoA psi s

keep :: a -> (a :+: b) :+: c
keep = L . L

make :: a -> (b :+: a) :+: c
make = L . R

next :: a -> b :+: (a :+: c)
next = R . L

