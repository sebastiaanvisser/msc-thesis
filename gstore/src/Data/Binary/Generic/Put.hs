{-# LANGUAGE FlexibleContexts #-}

module Data.Binary.Generic.Put (gput) where

import Data.Binary
import Generic.Core

class GBinary f where
  fput :: (a -> Put) -> f a -> Put

instance GBinary Id where
  fput f (Id r) = f r

instance Binary a => GBinary (K a) where
  fput _ (K x) = put x

instance GBinary Unit where
  fput _ Unit = put ()

instance (GBinary f, GBinary g) => GBinary (Sum f g) where
  fput f (Inl x) = put True  >> fput f x
  fput f (Inr x) = put False >> fput f x

instance (GBinary f, GBinary g) => GBinary (Prod f g) where
  fput f (Prod x y) = fput f x >> fput f y

instance GBinary f => GBinary (Con f) where
  fput f (Con _ x) = fput f x

gput :: (GBinary (PF a), PFView a) => a -> Put
gput = fput gput . from

