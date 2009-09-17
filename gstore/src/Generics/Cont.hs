module Generics.Cont
  ( Query
  , Produce
  , Modify

  , mkQuery
  , mkProducer
  , mkModifier
  )
where

import Control.Monad
import Control.Arrow
import Generics.Representation
import qualified Generics.Annotation as A

qC :: A.Annotation a f m => FixT a f -> m (f (FixT a f))
qC = runKleisli A.query . out

pC :: A.Annotation a f m => f (FixT a f) -> m (FixT a f)
pC = liftM In . runKleisli A.produce

type Q a f m c = FixT a f -> m c
type P a f m   = f (FixT a f) -> m (FixT a f)

type Query   a f m c = Q a f m c          -> f (FixT a f) -> m c
type Produce a f m   = P a f m            -> m (FixT a f)
type Modify  a f m   = Q a f m (FixT a f)
                    -> P a f m            -> f (FixT a f) -> m (FixT a f)

mkQuery
  :: A.Annotation a f m
  => Query a f m c
  -> FixT a f -> m c
mkQuery q = qC >=> fix (q . (qC >=>))

mkProducer
  :: A.Annotation a f m
  => Produce a f m
  -> m (FixT a f)
mkProducer c = c pC

mkModifier
  :: A.Annotation a f m
  => Modify a f m
  -> FixT a f -> m (FixT a f)
mkModifier m = qC >=> fix (flip m pC . (qC >=>))

