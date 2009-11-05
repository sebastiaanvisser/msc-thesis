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

data Para (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi  :: ((f r, f (FixT a f)) -> r) -> Para a f r
  Proj :: Para a f (r -> s, r, s) -> Para a f s

type ParaA f r = forall a. Para a f r

instance Functor f => Functor (Para a f) where
  fmap f psi = Proj (pure f <++> psi)

instance Functor f => Applicative (Para a f) where
  pure    = Psi . const
  a <*> b = Proj (a <++> b)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

(<++>) :: (Functor f, Functor (Para a f)) => Para a f (r -> s) -> Para a f r -> Para a f (r -> s, r, s)
Proj f <++> Proj g = fmap trd3 f <++> fmap trd3 g 
Psi  f <++> Proj g = Proj (pure id <++> Psi f) <++> Proj g
Proj f <++> Psi  g = Proj f <++> Proj (pure id <++> Psi g)
Psi  f <++> Psi  g = Psi (\(a, b) -> f (fmap fst3 a, b) `mk` g (fmap snd3 a, b))
  where mk x y = (x, y, x y)

_para :: (Traversable f, Lazy m, AnnQ a f m) => (x -> m r) -> (r -> x) -> Para a f x -> FixT a f -> m r
_para z y (Proj psi) f = trd3 <$> _para (\(a, b, r) -> z r >>= \r' -> return (a, b, r')) (\(a, b, r) -> (a, b, y r)) psi f
_para z y (Psi psi) f = 
  do g <- runQuery f
     r <- mapM (fmap y . lazy . _para z y (Psi psi)) g
     z (psi (r, g))

paraMT :: (AnnQ a f m, Lazy m, Traversable f) => Para a f r -> FixT a f -> m r
paraMT psi = _para return id psi

paraM :: (Applicative m, Monad m, Lazy m, Traversable f) => Para Id f r -> Fix f -> m r
paraM = paraMT 

paraT :: (AnnQ a f Identity, Traversable f) => Para a f c -> FixT a f -> c
paraT psi = runIdentity . paraMT psi

para :: Traversable f => Para Id f c -> Fix f -> c
para psi = runIdentity . paraM psi

paraMT' :: (DSeq r, Traversable f, Lazy m, AnnQ a f m) => Para a f r -> FixT a f -> m r
paraMT' psi f = dseqId <$> paraMT psi f

paraM' :: (DSeq r, Traversable f, Applicative m, Monad m, Lazy m) => Para Id f r -> Fix f -> m r
paraM' = paraMT'

paraT' :: (DSeq c, Traversable f, AnnQ a f Identity) => Para a f c -> FixT a f -> c
paraT' psi = runIdentity . paraMT' psi

para' :: (DSeq c, Traversable f) => Para Id f c -> Fix f -> c
para' psi = runIdentity . paraM' psi

type Endo a f = Para a f (FixT a f :+: f (FixT a f))
type EndoA f = forall a. Endo a f

toEndo :: Functor f => Para a f (FixT a f) -> Endo a f
toEndo = fmap Left

endoMT :: (Traversable f, Lazy m, AnnQ a f m, AnnP a f m) => Endo a f -> FixT a f -> m (FixT a f)
endoMT psi = _para (return `either` runProduce) Left psi

endoM :: (Traversable f, Lazy m, Applicative m, Monad m) => Endo Id f -> Fix f -> m (Fix f)
endoM = endoMT

endoT :: (Traversable f, AnnQ a f Identity, AnnP a f Identity) => Endo a f -> FixT a f -> FixT a f
endoT psi = runIdentity . endoMT psi

endo :: Traversable f => Endo Id f -> Fix f -> Fix f
endo psi = runIdentity . endoM psi


