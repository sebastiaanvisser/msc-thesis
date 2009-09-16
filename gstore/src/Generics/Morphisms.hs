module Generics.Morphisms where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Data.Traversable
import Generics.Annotation
import Generics.Representation

instance Monad m => Functor (Kleisli m a) where
  fmap f (Kleisli m) = Kleisli (liftM f . m)

-- | Apomorphism for functors with annotated fixpoint in an monadic context.

type Seed s a f = (s, f (FixT a f))
type Stop   a f = Either (FixT a f) (f (FixT a f))
type Psi' s a f = Seed s a f -> f (Either (Stop a f) (s, Stop a f))
type Psi  s   f = forall a. Psi' s a f

-- TODO: Aspect (renamed to annotation) should already have FixT in it!!)

apoT
  :: (Traversable f, Applicative m, Annotation a f (FixT a f) m)
  => Psi' s a f -> s -> a f (FixT a f) -> m (a f (FixT a f))
apoT psi s = runKleisli . modify . Kleisli $ sequenceA . fmap rec . psi . (,) s
  where
  rec e =
    case e of
      Left      (Left  x)  -> return x
      Left      (Right x)  -> In <$> runKleisli produce x
      Right (t, (Left  x)) -> In <$> apoT psi t (out x)
      Right (t, (Right x)) -> In <$> (runKleisli produce x >>= apoT psi t)

-- | Apomorphism for functors with fixpoint in an applicative context.

apoA
  :: (Traversable f, Applicative m, Monad m)
  => Psi' s Id f -> s -> Fix f -> m (Fix f)
apoA psi s = return . In <=< apoT psi s . out

-- | Plain apomorphism for functors with fixpoint.

apo :: Traversable f => Psi' s Id f -> s -> Fix f -> Fix f
apo psi s = runIdentity . apoA psi s

