module Generics.Morphism.Para where

import Annotation.Annotation
import Control.Applicative
import Control.Category
import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Control.Monad.Lazy
import Data.Traversable
import Generics.Regular.Seq
import Generics.Types
import Prelude hiding ((.), id, mapM)

data AlgA (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi  :: (f (FixA a f, r) -> r)  -> AlgA a f r
  Proj :: AlgA a f (r -> s, r, s) -> AlgA a f s

type Alg f r = forall a. AlgA a f r

instance Functor f => Functor (AlgA a f) where
  fmap f psi = Proj (pure f <++> psi)

instance Functor f => Applicative (AlgA a f) where
  pure    = Psi . const
  a <*> b = Proj (a <++> b)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

(<++>) :: forall f r a s. (Functor f, Functor (AlgA a f)) => AlgA a f (r -> s) -> AlgA a f r -> AlgA a f (r -> s, r, s)
Proj f <++> Proj g = fmap trd3 f <++> fmap trd3 g 
Psi  f <++> Proj g = Proj (pure id <++> Psi f) <++> Proj g
Proj f <++> Psi  g = Proj f <++> Proj (pure id <++> Psi g)
Psi  f <++> Psi  g = Psi (\x -> f (fmap2 fst3 x) `mk` g (fmap2 snd3 x))
  where mk x y = (x, y, x y)

_para :: forall f m a r x. (Traversable f, Lazy m, AnnQ a f m) => (x -> m r) -> (r -> x) -> AlgA a f x -> FixA a f -> m r
_para z y (Proj psi) = fmap trd3 . _para (\(a, b, r) -> z r >>= \r' -> return (a, b, r')) (\(a, b, r) -> (a, b, y r)) psi
_para z y (Psi psi)  = z . psi <=< mapM (g (fmap y . lazy . _para z y (Psi psi))) <=< runQuery
  where g f c = fmap ((,) c) (f c)

-- Lazy paramorphism in a monadic context for annotated structures.

paraMA :: (AnnQ a f m, Lazy m, Traversable f) => AlgA a f r -> FixA a f -> m r
paraMA psi = _para return id psi

-- Lazy paramorphism in a monadic context for structures without annotations.

paraM :: (Applicative m, Monad m, Lazy m, Traversable f) => AlgA Id f r -> Fix f -> m r
paraM = paraMA 

-- Lazy paramorphism for annotated structures.

paraA :: (AnnQ a f Identity, Traversable f) => AlgA a f c -> FixA a f -> c
paraA psi = runIdentity . paraMA psi

-- Lazy paramorphism for structures without annotations.

para :: Traversable f => AlgA Id f c -> Fix f -> c
para psi = runIdentity . paraM psi

-- Strict paramorphism in a monadic context for annotated structures.

paraMA' :: (DSeq r, Traversable f, Lazy m, AnnQ a f m) => AlgA a f r -> FixA a f -> m r
paraMA' psi f = dseqId <$> paraMA psi f

-- Strict paramorphism in a monadic context for structures without annotations.

paraM' :: (DSeq r, Traversable f, Applicative m, Monad m, Lazy m) => AlgA Id f r -> Fix f -> m r
paraM' = paraMA'

-- Strict paramorphism for annotated structures.

paraA' :: (DSeq c, Traversable f, AnnQ a f Identity) => AlgA a f c -> FixA a f -> c
paraA' psi = runIdentity . paraMA' psi

-- Strict paramorphism for structures without annotations.

para' :: (DSeq c, Traversable f) => AlgA Id f c -> Fix f -> c
para' psi = runIdentity . paraM' psi

type EndoA a f = AlgA a f (FixA a f :+: f (FixA a f))
type Endo f = forall a. EndoA a f

toEndo :: Functor f => AlgA a f (FixA a f) -> EndoA a f
toEndo = fmap Left

endoMA :: (Traversable f, Lazy m, AnnQ a f m, AnnP a f m) => EndoA a f -> FixA a f -> m (FixA a f)
endoMA psi = _para (return `either` runProduce) Left psi

endoM :: (Traversable f, Lazy m, Applicative m, Monad m) => EndoA Id f -> Fix f -> m (Fix f)
endoM = endoMA

endoA :: (Traversable f, AnnQ a f Identity, AnnP a f Identity) => EndoA a f -> FixA a f -> FixA a f
endoA psi = runIdentity . endoMA psi

endo :: Traversable f => EndoA Id f -> Fix f -> Fix f
endo psi = runIdentity . endoM psi


