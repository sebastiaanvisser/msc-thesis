module Generics.Morphism.Cata where

import Annotation.Annotation
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Lazy
import Data.Traversable
import Generics.Morphism.Para (Para, paraMT, paraM, paraT, para)
import Generics.Types
import qualified Generics.Morphism.Para as Para

data Cata (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi  :: (f r -> r) -> Cata a f r
  Proj :: Cata a f (r -> s, r, s) -> Cata a f s

cataToPara :: Cata a f r -> Para a f r
cataToPara (Psi  c) = Para.Psi (c . fst)
cataToPara (Proj p) = Para.Proj (cataToPara p)

cataMT :: (AnnQ a f m, Lazy m, Traversable f) => Cata a f r -> FixT a f -> m r
cataMT = paraMT . cataToPara

cataM :: (Applicative m, Monad m, Lazy m, Traversable f) => Cata Id f r -> Fix f -> m r
cataM = paraM . cataToPara

cataT :: (AnnQ a f Identity, Traversable f) => Cata a f c -> FixT a f -> c
cataT = paraT . cataToPara

cata :: Traversable f => Cata Id f c -> Fix f -> c
cata = para . cataToPara

