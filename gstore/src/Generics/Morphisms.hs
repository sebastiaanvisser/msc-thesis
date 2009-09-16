module Generics.Morphisms where

import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Data.Traversable
import Generics.Aspect
import Generics.Representation
import Prelude hiding (sequence)

-- | Apomorphism for functors with annotated fixpoint in an monadic context.

type Seed s a f = (s, f (FixT a f))
type Stop   a f = Either (FixT a f) (f (FixT a f))
type Psi' s a f = Seed s a f -> f (Either (Stop a f) (s, Stop a f))
type Psi  s   f = forall a. Psi' s a f

apoT
  :: (Traversable f, Applicative m, Aspect a f (FixT a f) m)
  => Psi' s a f -> s -> a f (FixT a f) -> m (a f (FixT a f))
apoT psi s = produce <=< sequenceA . fmap my . psi . (,) s <=< query
  where
  my e =
    case e of
      Left      (Left  x)  -> return x
      Left      (Right x)  -> In <$> produce x
      Right (t, (Left  x)) -> In <$> apoT psi t (out x)
      Right (t, (Right x)) -> In <$> (produce x >>= apoT psi t)

-- | Apomorphism for functors with fixpoint in an applicative context.

apoA
  :: (Traversable f, Applicative m, Monad m)
  => Psi' s Id f -> s -> Fix f -> m (Fix f)
apoA psi s = return . In <=< apoT psi s . out

-- | Plain apomorphism for functors with fixpoint.

apo :: Traversable f => Psi' s Id f -> s -> Fix f -> Fix f
apo psi s = runIdentity . apoA psi s

