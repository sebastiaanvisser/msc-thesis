module Generics.Morphism.Cata where

import Annotation.Annotation
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Lazy
import Data.Traversable
import Generics.Types
import qualified Generics.Morphism.Para as Para

data AlgA (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi  :: (f r -> r) -> AlgA a f r
  Proj :: AlgA a f (r -> s, r, s) -> AlgA a f s

type Alg f r = forall a. AlgA a f r

cataToPara :: AlgA a f r -> Para.AlgA a f r
cataToPara (Psi  c) = Para.Psi (c . fst)
cataToPara (Proj p) = Para.Proj (cataToPara p)

cataMA :: (AnnQ a f m, Lazy m, Traversable f) => AlgA a f r -> FixA a f -> m r
cataMA = Para.paraMA . cataToPara

cataM :: (Applicative m, Monad m, Lazy m, Traversable f) => AlgA Id f r -> Fix f -> m r
cataM = Para.paraM . cataToPara

cataA :: (AnnQ a f Identity, Traversable f) => AlgA a f c -> FixA a f -> c
cataA = Para.paraA . cataToPara

cata :: Traversable f => AlgA Id f c -> Fix f -> c
cata = Para.para . cataToPara

