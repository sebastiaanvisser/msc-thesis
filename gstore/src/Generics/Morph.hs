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

-- Paramorphism with strict output.

paraMT' :: (DSeq r, Traversable f, Lazy m, AnnQ a f m) => Psi a f r -> FixT1 a f -> m r
paraMT' psi f = dseqId <$> paraMT psi f

-- Paramorphisms that output to the same structure as comes in.

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

-- | Apomorphism for functors with annotated fixpoint in an monadic context.

type Phi  s a f = s -> f (s :+: f (FixT a f))
type PhiA s   f = forall a. Phi s a f

apoT :: (Traversable f, AnnM a f m) => Phi s a f -> s -> m (FixT1 a f)
apoT phi = runProduce <=< sequence . fmap (fmap In . (apoT phi `sum` runProduce)) . phi

-- Apomorphism that includes the structure type itself in its seed.

type Seed    s a f = s :*: f (FixT a f)
type Stop      a f = FixT a f :+: f (FixT a f)
type Next    s a f = s :*: FixT a f :+: s :*: f (FixT a f)
type CoEndo  s a f = Seed s a f -> f (Stop a f :+: Next s a f)
type CoEndoA s   f = forall a. CoEndo s a f

coEndoMT
  :: (Traversable f, AnnM a f m)
  => CoEndo s a f -> s -> FixT1 a f -> m (FixT1 a f)
coEndoMT phi s = runKleisli . modify $ Kleisli (sequence . fmap rec . phi . (,) s)
  where
  rec e =
    case e of
      L (L x)      -> return x
      L (R x)      -> In <$> runProduce x
      R (L (t, x)) -> In <$> coEndoMT phi t (out x)
      R (R (t, x)) -> In <$> (runProduce x >>= coEndoMT phi t)

-- | Apomorphism for functors with fixpoint in an applicative context.

coEndoM :: (Traversable f, Applicative m, Monad m) => CoEndo s Id f -> s -> Fix f -> m (Fix f)
coEndoM phi s = return . In <=< coEndoMT phi s . out

coEndoT :: (Traversable f, AnnM a f Identity) => CoEndo s a f -> s -> FixT1 a f -> FixT1 a f
coEndoT phi s = runIdentity . coEndoMT phi s

-- | Plain apomorphism for functors with fixpoint.

coEndo :: Traversable f => CoEndo s Id f -> s -> Fix f -> Fix f
coEndo phi s = runIdentity . coEndoM phi s

keep :: a -> (a :+: b) :+: c
keep = L . L

make :: a -> (b :+: a) :+: c
make = L . R

next :: a -> b :+: (a :+: c)
next = R . L

