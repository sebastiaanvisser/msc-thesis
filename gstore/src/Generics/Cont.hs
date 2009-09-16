module Generics.Cont
  ( QueryC
  , ProduceC
  , ModifyC

  , mkQuery
  , mkProducer
  , mkModifier
  )
where

import Control.Monad
import Control.Arrow
import Generics.Annotation
import Generics.Representation

qC :: Annotation a f m => FixT a f -> m (f (FixT a f))
qC = runKleisli query . out

pC :: Annotation a f m => f (FixT a f) -> m (FixT a f)
pC = liftM In . runKleisli produce

type Q a f m c = FixT a f -> m c
type P a f m   = f (FixT a f) -> m (FixT a f)

type QueryC   a f m c = Q a f m c                     -> f (FixT a f) -> m c
type ProduceC a f m   = P a f m                                       -> m (FixT a f)
type ModifyC  a f m   = Q a f m (FixT a f) -> P a f m -> f (FixT a f) -> m (FixT a f)

mkQuery
  :: Annotation a f m
  => QueryC a f m c
  -> FixT a f -> m c
mkQuery q = qC >=> fix (q . (qC >=>))

mkProducer
  :: Annotation a f m
  => ProduceC a f m
  -> m (FixT a f)
mkProducer c = c pC

mkModifier
  :: Annotation a f m
  => ModifyC a f m
  -> FixT a f -> m (FixT a f)
mkModifier m = qC >=> fix (flip m pC . (qC >=>))

