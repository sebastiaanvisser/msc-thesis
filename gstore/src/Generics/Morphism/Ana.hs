module Generics.Morphism.Ana where

import Annotation.Annotation
import Control.Applicative
import Control.Monad.Identity
import Data.Traversable
import Generics.Types
import qualified Generics.Morphism.Apo as Apo

data CoalgA (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f s) -> CoalgA a f s

type Coalg s f = forall a. CoalgA a f s

anaToApo :: Functor f => CoalgA a f s -> Apo.CoalgA a f s
anaToApo (Phi s) = Apo.Phi (fmap Left . s)

anaMA :: (Traversable f, AnnM a f m) => CoalgA a f s -> s -> m (FixA a f)
anaMA = Apo.apoMA . anaToApo

anaM :: (Traversable f, Applicative m, Monad m) => CoalgA Id f s -> s -> m (Fix f)
anaM = Apo.apoM . anaToApo

anaA :: (Traversable f, AnnM a f Identity) => CoalgA a f s -> s -> FixA a f
anaA = Apo.apoA . anaToApo

ana :: Traversable f => CoalgA Id f s -> s -> Fix f
ana = Apo.apo . anaToApo

