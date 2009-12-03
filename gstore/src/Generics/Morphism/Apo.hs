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

data CoalgA (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f (s :+: FixA a f)) -> CoalgA a f s

type Coalg s f = forall a. CoalgA a f s

apoMA :: (Traversable f, AnnM a f m) => CoalgA a f s -> s -> m (FixA a f)
apoMA (Phi phi) = runProduce <=< mapM (apoMA (Phi phi) `either` return) . phi

apoM :: (Traversable f, AnnM Id f m) => CoalgA Id f s -> s -> m (Fix f)
apoM = apoMA

apoA :: (Traversable f, AnnM a f Identity) => CoalgA a f s -> s -> FixA a f
apoA phi = runIdentity . apoMA phi

apo :: Traversable f => CoalgA Id f s -> s -> Fix f
apo phi = runIdentity . apoM phi

type EndoA a f = f (FixA a f) -> f (FixA a f :+: (FixA a f :+: f (FixA a f)))
type Endo    f = forall a. EndoA a f

endoMA
  :: (Traversable f, AnnM a f m)
  => EndoA a f -> FixA a f -> m (FixA a f)
endoMA phi = runKleisli ((modify . Kleisli) (mapM cont . phi))
  where
  cont (Left x)          = endoMA phi x
  cont (Right (Left  x)) = return x
  cont (Right (Right x)) = runProduce x

endoM :: (Traversable f, Applicative m, Monad m) => EndoA Id f -> Fix f -> m (Fix f)
endoM = endoMA

endoA :: (Traversable f, AnnM a f Identity) => EndoA a f -> FixA a f -> FixA a f
endoA phi = runIdentity . endoMA phi

endo :: Traversable f => EndoA Id f -> Fix f -> Fix f
endo phi = runIdentity . endoM phi

