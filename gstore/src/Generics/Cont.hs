module Generics.Cont
( Query
, Produce
, Modify
, mkQuery
, mkProducer
, mkModifier
)
where

import Control.Arrow
import Control.Monad
import Generics.Types
import qualified Annotation.Annotation as A

type Q a f m c = FixA a f -> m c
type P a f m   = f (FixA a f) -> m (FixA a f)

type Query   a f m c = Q a f m c          -> f (FixA a f) -> m c
type Produce a f m   = P a f m            -> m (FixA a f)
type Modify  a f m   = Q a f m (FixA a f)
                    -> P a f m            -> f (FixA a f) -> m (FixA a f)

mkQuery :: A.AnnQ a f m => Query a f m c -> FixA1 a f -> m c
mkQuery q = fix (q . (<=< qC)) <=< qC . In

mkProducer :: A.AnnP a f m => Produce a f m -> m (FixA1 a f)
mkProducer p = out `liftM` p pC

mkModifier :: A.AnnM a f m => Modify a f m -> FixA1 a f -> m (FixA1 a f)
mkModifier m = liftM out . fix (flip m pC . (<=< qC)) <=< qC . In

qC :: A.AnnQ a f m => FixA a f -> m (f (FixA a f))
qC = runKleisli A.query

pC :: A.AnnP a f m => f (FixA a f) -> m (FixA a f)
pC = runKleisli A.produce

