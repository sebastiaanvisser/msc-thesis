module Generics.Morph where

import Annotation.Annotation
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad hiding (sequence)
import Control.Monad.Identity
import Control.Monad.Lazy
import Data.Traversable
import Generics.Regular.Seq
import Generics.Types
import Prelude hiding ((.), id, sequence, sum)

data Psi (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi :: ((f r, f (FixT a f)) -> r) -> Psi a f r
  Prj :: Psi a f (r -> s, r, s) -> Psi a f s

type PsiA f r = forall a. Psi a f r

instance Functor f => Functor (Psi a f) where
  fmap f psi = Prj (pure f <++> psi)

instance Functor f => Applicative (Psi a f) where
  pure    = Psi . const
  a <*> b = Prj (a <++> b)

idPsi :: Functor f => Psi a f (r -> r)
idPsi = pure id

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

(<++>)
  :: (Functor f, Functor (Psi a f))
  => Psi a f (r -> s)
  -> Psi a f r
  -> Psi a f (r -> s, r, s)
Prj f <++> Prj g = fmap trd3 f <++> fmap trd3 g 
Psi f <++> Prj g = Prj (idPsi <++> Psi f) <++> Prj g
Prj f <++> Psi g = Prj f <++> Prj (idPsi <++> Psi g)
Psi f <++> Psi g = Psi (\(a, b) -> f (fmap fst3 a, b) `mk` g (fmap snd3 a, b))
  where mk x y = (x, y, x y)

_para :: (Traversable f, Lazy m, AnnQ a f m) => (x -> m r) -> (r -> x) -> Psi a f x -> FixT1 a f -> m r
_para z y (Prj psi) f = trd3 <$> _para (\(a, b, r) -> z r >>= \r' -> return (a, b, r')) (\(a, b, r) -> (a, b, y r)) psi f
_para z y (Psi psi) f = 
  do g <- runQuery f
     r <- fmap y <$> sequence (fmap (lazy . _para z y (Psi psi) . out) g)
     z (psi (r, g))

paraMT :: (AnnQ a f m, Lazy m, Traversable f) => Psi a f r -> FixT1 a f -> m r
paraMT = _para return id

paraMT' :: (DSeq r, Traversable f, Lazy m, AnnQ a f m) => Psi a f r -> FixT1 a f -> m r
paraMT' psi f = dseqId <$> paraMT psi f

type Endo a f = Psi a f (FixT a f :+: f (FixT a f))
type EndoA f = forall a. Endo a f

toEndo :: Functor f => Psi a f (FixT a f) -> Endo a f
toEndo = fmap L

endoMT :: (Traversable f, Lazy m, AnnQ a f m, AnnP a f m) => Endo a f -> FixT1 a f -> m (FixT1 a f)
endoMT = _para ((return . out) `sum` runProduce) (L . In)

endoM :: (Traversable f, Lazy m, Applicative m, Monad m) => Endo Id f -> Fix f -> m (Fix f)
endoM psi = return . In <=< endoMT psi . out

endoT :: (Traversable f, AnnQ a f Identity, AnnP a f Identity) => Endo a f -> FixT1 a f -> FixT1 a f
endoT psi = runIdentity . endoMT psi

endo :: Traversable f => Endo Id f -> Fix f -> Fix f
endo psi = runIdentity . endoM psi

-------------------------------------------------------------------------------

data Phi (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f (s :+: f (FixT a f))) -> Phi a f s

type PhiA s f = forall a. Phi a f s

apoMT :: (Traversable f, AnnM a f m) => Phi a f s -> s -> m (FixT1 a f)
apoMT (Phi phi) = runProduce <=< sequence . fmap (fmap In . (apoMT (Phi phi) `sum` runProduce)) . phi


type Seed    s a f = s :*: f (FixT a f)
type Stop      a f = FixT a f :+: f (FixT a f)
type Next    s a f = s :*: (FixT a f :+: f (FixT a f))
type CoEndo  s a f = Seed s a f -> f (Stop a f :+: Next s a f)
type CoEndoA s   f = forall a. CoEndo s a f

coEndoMT
  :: (Traversable f, AnnM a f m)
  => CoEndo s a f -> s -> FixT1 a f -> m (FixT1 a f)
coEndoMT phi s = runKleisli . modify $ Kleisli (sequence . fmap rec . phi . (,) s)
  where
  rec (L x     ) = sum return (fmap In . runProduce) x
  rec (R (t, x)) = In <$> sum (coEndoMT phi t . out) (coEndoMT phi t <=< runProduce) x

coEndoM :: (Traversable f, Applicative m, Monad m) => CoEndo s Id f -> s -> Fix f -> m (Fix f)
coEndoM phi s = return . In <=< coEndoMT phi s . out

coEndoT :: (Traversable f, AnnM a f Identity) => CoEndo s a f -> s -> FixT1 a f -> FixT1 a f
coEndoT phi s = runIdentity . coEndoMT phi s

coEndo :: Traversable f => CoEndo s Id f -> s -> Fix f -> Fix f
coEndo phi s = runIdentity . coEndoM phi s

keep :: a -> (a :+: b) :+: c
keep = L . L

make :: a -> (b :+: a) :+: c
make = L . R

next :: (s, b) -> a :+: (s, b :+: c)
next (a, b) = R (a, L b)

