module Generics.Morphisms where

import Control.Arrow
import Control.Category
import Control.Monad hiding (sequence)
import Control.Monad.Identity
import Data.Traversable
import Annotation.Annotation
import Generics.Representation
import Prelude hiding ((.), id, sequence)

-- | Apomorphism for functors with annotated fixpoint in an monadic context.

type Seed s a f = (s, f (FixT a f))
type Stop   a f = Either (FixT a f) (f (FixT a f))
type Psi' s a f = Seed s a f -> f (Either (Stop a f) (s, Stop a f))
type Psi  s   f = forall a. Psi' s a f

apoT
  :: (Traversable f, Annotation a f m)
  => Psi' s a f -> s -> a f (FixT a f) -> m (a f (FixT a f))
apoT psi s = runKleisli . modify . Kleisli $ sequence . fmap rec . psi . (,) s
  where
  rec e =
    case e of
      Left      (Left  x)  -> return x
      Left      (Right x)  -> In `liftM` runKleisli produce x
      Right (t, (Left  x)) -> In `liftM` apoT psi t (out x)
      Right (t, (Right x)) -> In `liftM` (runKleisli produce x >>= apoT psi t)

-- | Apomorphism for functors with fixpoint in an applicative context.

apoA
  :: (Traversable f, Monad m)
  => Psi' s Id f -> s -> Fix f -> m (Fix f)
apoA psi s = return . In <=< apoT psi s . out

-- | Plain apomorphism for functors with fixpoint.

apo :: Traversable f => Psi' s Id f -> s -> Fix f -> Fix f
apo psi s = runIdentity . apoA psi s

