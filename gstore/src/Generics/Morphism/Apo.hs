module Generics.Morphism.Apo where

import Annotation.Annotation
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad hiding (sequence)
import Control.Monad.Identity
import Data.Traversable
import Generics.Types
import Prelude hiding ((.), id, sequence)

data Phi (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f (s :+: f (FixT a f))) -> Phi a f s

type PhiA s f = forall a. Phi a f s

apoMT :: (Traversable f, AnnM a f m) => Phi a f s -> s -> m (FixT a f)
apoMT (Phi phi) = fmap In . runProduce <=< sequence . fmap (apoMT (Phi phi) `either` (fmap In . runProduce)) . phi

apoM :: (Traversable f, AnnM Id f m) => Phi Id f s -> s -> m (Fix f)
apoM = apoMT

apoT :: (Traversable f, AnnM a f Identity) => Phi a f s -> s -> FixT a f
apoT phi = runIdentity . apoMT phi

apo :: Traversable f => Phi Id f s -> s -> Fix f
apo phi = runIdentity . apoM phi

type CoEndo a f = f (FixT a f) -> f (FixT a f :+: (FixT a f :+: f (FixT a f)))
type CoEndoA  f = forall a. CoEndo a f

coEndoMT
  :: (Traversable f, AnnM a f m)
  => CoEndo a f -> FixT a f -> m (FixT a f)
coEndoMT phi = fmap In . runKleisli ((modify . Kleisli) (sequence . fmap cont . phi)) . out
  where
  cont (Left x)          = coEndoMT phi x
  cont (Right (Left  x)) = return x
  cont (Right (Right x)) = In <$> runProduce x

coEndoM :: (Traversable f, Applicative m, Monad m) => CoEndo Id f -> Fix f -> m (Fix f)
coEndoM = coEndoMT

coEndoT :: (Traversable f, AnnM a f Identity) => CoEndo a f -> FixT a f -> FixT a f
coEndoT phi = runIdentity . coEndoMT phi

coEndo :: Traversable f => CoEndo Id f -> Fix f -> Fix f
coEndo phi = runIdentity . coEndoM phi

