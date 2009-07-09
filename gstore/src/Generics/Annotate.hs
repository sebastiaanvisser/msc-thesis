{-# LANGUAGE
    FlexibleContexts
  , UndecidableInstances
  , TypeOperators
 #-}
module Generics.Annotate where

import Control.Monad
import Data.Binary
import Generics.Aspect
import Generics.Regular.Base
import Generics.Representation

instance Binary (f (g a)) => Binary ((f :. g) a) where
  get = O `fmap` get
  put = put . unO

type Producer f g m   = (f g -> m g) -> m g
type Query    f g m c = (g -> m c) -> f g -> m c
type Modifier f g m c = (f g -> m g) -> (g -> m c) -> f g -> m c

mkProducer
  :: Aspect g (AnnFix f g) m
  => Producer f (AnnFixF f g) m
  -> m (AnnFixF f g)
mkProducer p = p (produce . In . O) 

mkQuery
  :: Aspect g (AnnFix f g) m
  => Query f (AnnFixF f g) m c
  -> AnnFixF f g -> m c
mkQuery q f = liftM (unO . out) (query f) >>= w
  where w = q (query >=> w . unO . out)

mkModifier
  :: Aspect g (AnnFix f g) m
  => Modifier f (AnnFixF f g) m c
  -> AnnFixF f g -> m c
mkModifier m = q >=> w . unO . out
  where w = m (p . In . O) (q >=> w . unO . out)
        (p, q) = modify

