module Generics.Morphism.Ana where

import Annotation.Annotation
import Control.Applicative
import Control.Monad.Identity
import Data.Traversable
import Generics.Morphism.Apo (Apo, apoMT, apoM, apoT, apo)
import Generics.Types
import qualified Generics.Morphism.Apo as Apo

data Ana (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f s) -> Ana a f s

type AnaA s f = forall a. Ana a f s

anaToApo :: Functor f => Ana a f s -> Apo a f s
anaToApo (Phi s) = Apo.Phi (fmap Left . s)

anaMT :: (Traversable f, AnnM a f m) => Ana a f s -> s -> m (FixT a f)
anaMT = apoMT . anaToApo

anaM :: (Traversable f, Applicative m, Monad m) => Ana Id f s -> s -> m (Fix f)
anaM = apoM . anaToApo

anaT :: (Traversable f, AnnM a f Identity) => Ana a f s -> s -> FixT a f
anaT = apoT . anaToApo

ana :: Traversable f => Ana Id f s -> s -> Fix f
ana = apo . anaToApo

