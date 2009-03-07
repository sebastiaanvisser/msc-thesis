{-# LANGUAGE
    FlexibleContexts
  , UndecidableInstances
  , TypeOperators
 #-}
module Generic.Annotate where

import Control.Monad
import Data.Binary
import Generic.Aspect
import Generic.Representation

instance Binary (f (g a)) => Binary ((f :. g) a) where
  get = C `fmap` get
  put = put . unC

type Producer f g m   = (f g -> m g) -> m g
type Query    f g m c = (g -> m c) -> f g -> m c
type Modifier f g m c = (f g -> m g) -> (g -> m c) -> f g -> m c

mkProducer
  :: Aspect g (AnnFix f g) m
  => Producer f (AnnFixF f g) m
  -> m (AnnFixF f g)

mkProducer p = p (produce . In . C) 

mkQuery
  :: Aspect g (AnnFix f g) m
  => Query f (AnnFixF f g) m c
  -> AnnFixF f g -> m c

mkQuery q f = liftM (unC . out) (query f) >>= w
  where w = q (query >=> w . unC . out)

mkModifier
  :: Aspect g (AnnFix f g) m
  => Modifier f (AnnFixF f g) m c
  -> AnnFixF f g -> m c

mkModifier m = q >=> w . unC . out
  where w = m (p . In . C) (q >=> w . unC . out)
        (p, q) = modify

