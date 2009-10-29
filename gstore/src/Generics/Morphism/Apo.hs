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

apoMT :: (Traversable f, AnnM a f m) => Phi a f s -> s -> m (FixT1 a f)
apoMT (Phi phi) = runProduce <=< sequence . fmap (fmap In . (apoMT (Phi phi) `either` runProduce)) . phi

type CoEndo a f = f (FixT a f) -> f (FixT a f :+: (FixT a f :+: f (FixT a f)))
type CoEndoA  f = forall a. CoEndo a f

coEndoMT
  :: (Traversable f, AnnM a f m)
  => CoEndo a f -> FixT1 a f -> m (FixT1 a f)
coEndoMT phi = runKleisli . modify $ Kleisli (sequence . fmap cont . phi)
  where
  cont (Left x)          = In <$> coEndoMT phi (out x)
  cont (Right (Left  x)) = return x
  cont (Right (Right x)) = In <$> runProduce x

coEndoM :: (Traversable f, Applicative m, Monad m) => CoEndo Id f -> Fix f -> m (Fix f)
coEndoM phi = return . In <=< coEndoMT phi . out

coEndoT :: (Traversable f, AnnM a f Identity) => CoEndo a f -> FixT1 a f -> FixT1 a f
coEndoT phi = runIdentity . coEndoMT phi

coEndo :: Traversable f => CoEndo Id f -> Fix f -> Fix f
coEndo phi = runIdentity . coEndoM phi

