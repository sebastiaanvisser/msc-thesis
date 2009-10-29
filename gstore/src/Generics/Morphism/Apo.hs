module Generics.Morphism.Apo where

import Annotation.Annotation
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad hiding (sequence)
import Control.Monad.Identity
import Data.Traversable
import Generics.Types
import Prelude hiding ((.), id, sequence, sum)

data Phi (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f (s :+: f (FixT a f))) -> Phi a f s

type PhiA s f = forall a. Phi a f s

apoMT :: (Traversable f, AnnM a f m) => Phi a f s -> s -> m (FixT1 a f)
apoMT (Phi phi) = runProduce <=< sequence . fmap (fmap In . (apoMT (Phi phi) `sum` runProduce)) . phi


type Seed    s a f = s :*: f (FixT a f)
type Stop      a f = FixT a f :+: f (FixT a f)
type Next    s a f = s :*: (FixT a f :+: f (FixT a f))
type CoEndo  s a f = Seed s a f -> f (Stop a f :+: Next s a f)
type CoEndoA s   f = forall a. CoEndo s a f

coEndoMT
  :: (Traversable f, AnnM a f m)
  => CoEndo s a f -> s -> FixT1 a f -> m (FixT1 a f)
coEndoMT phi s = runKleisli . modify $ Kleisli (sequence . fmap rec . phi . (,) s)
  where
  rec (L x     ) = sum return (fmap In . runProduce) x
  rec (R (t, x)) = In <$> sum (coEndoMT phi t . out) (coEndoMT phi t <=< runProduce) x

coEndoM :: (Traversable f, Applicative m, Monad m) => CoEndo s Id f -> s -> Fix f -> m (Fix f)
coEndoM phi s = return . In <=< coEndoMT phi s . out

coEndoT :: (Traversable f, AnnM a f Identity) => CoEndo s a f -> s -> FixT1 a f -> FixT1 a f
coEndoT phi s = runIdentity . coEndoMT phi s

coEndo :: Traversable f => CoEndo s Id f -> s -> Fix f -> Fix f
coEndo phi s = runIdentity . coEndoM phi s

keep :: a -> (a :+: b) :+: c
keep = L . L

make :: a -> (b :+: a) :+: c
make = L . R

next :: (s, b) -> a :+: (s, b :+: c)
next (a, b) = R (a, L b)


