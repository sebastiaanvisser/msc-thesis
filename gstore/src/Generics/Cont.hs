module Generics.Cont where

import Control.Monad
import Control.Arrow
import Generics.Annotation
import Generics.Representation

type Cont a f m =
  (    FixT a f  -> m (f (FixT a f))
  , f (FixT a f) -> m    (FixT a f)
  )

cont :: Annotation a f (FixT a f) m => Cont a f m
cont = (runKleisli query . out, liftM In . runKleisli produce)

type Q a f m c = FixT a f -> m c
type P a f m   = f (FixT a f) -> m (FixT a f)

type QueryC   a f m c = Q a f m c                     -> f (FixT a f) -> m c
type ProduceC a f m   = P a f m                                       -> m (FixT a f)
type ModifyC  a f m   = Q a f m (FixT a f) -> P a f m -> f (FixT a f) -> m (FixT a f)

mkProducer
  :: Annotation a f (FixT a f) m
  => ProduceC a f m
  -> m (FixT a f)
mkProducer c = c (snd cont)

mkQuery
  :: Annotation a f (FixT a f) m
  => QueryC a f m c
  -> FixT a f -> m c
mkQuery q = fst cont >=> fix (q . (fst cont >=>))

mkModifier
  :: Annotation a f (FixT a f) m
  => ModifyC a f m
  -> FixT a f -> m (FixT a f)
mkModifier m = fst cont >=> fix (flip m (snd cont) . (fst cont >=>))

