module Generics.Morphism.Apo where

import Annotation.Annotation
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Data.Traversable
import Generics.Types
import Prelude hiding ((.), id, mapM)

data Apo (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f (s :+: f (FixT a f))) -> Apo a f s

type ApoA s f = forall a. Apo a f s

apoMT :: (Traversable f, AnnM a f m) => Apo a f s -> s -> m (FixT a f)
apoMT (Phi phi) = runProduce <=< mapM (apoMT (Phi phi) `either` runProduce) . phi

apoM :: (Traversable f, AnnM Id f m) => Apo Id f s -> s -> m (Fix f)
apoM = apoMT

apoT :: (Traversable f, AnnM a f Identity) => Apo a f s -> s -> FixT a f
apoT phi = runIdentity . apoMT phi

apo :: Traversable f => Apo Id f s -> s -> Fix f
apo phi = runIdentity . apoM phi

type CoEndo a f = f (FixT a f) -> f (FixT a f :+: (FixT a f :+: f (FixT a f)))
type CoEndoA  f = forall a. CoEndo a f

coEndoMT
  :: (Traversable f, AnnM a f m)
  => CoEndo a f -> FixT a f -> m (FixT a f)
coEndoMT phi = runKleisli ((modify . Kleisli) (mapM cont . phi))
  where
  cont (Left x)          = coEndoMT phi x
  cont (Right (Left  x)) = return x
  cont (Right (Right x)) = runProduce x

coEndoM :: (Traversable f, Applicative m, Monad m) => CoEndo Id f -> Fix f -> m (Fix f)
coEndoM = coEndoMT

coEndoT :: (Traversable f, AnnM a f Identity) => CoEndo a f -> FixT a f -> FixT a f
coEndoT phi = runIdentity . coEndoMT phi

coEndo :: Traversable f => CoEndo Id f -> Fix f -> Fix f
coEndo phi = runIdentity . coEndoM phi

