{-# LANGUAGE FlexibleContexts #-}
module Data.Binary.Generic (gput, gget) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Generic.Core

class GBinary f where
  fput :: (a -> Put) -> f a -> Put
  fget :: Get a -> Get (f a)

instance GBinary Id where
  fput f (Id r) = f r
  fget f = Id <$> f

instance Binary a => GBinary (K a) where
  fput _ (K x) = put x
  fget _ = K <$> get

instance GBinary Unit where
  fput _ Unit = put ()
  fget _ = return Unit

instance (GBinary f, GBinary g) => GBinary (Sum f g) where
  fput f (Inl x) = put True  >> fput f x
  fput f (Inr x) = put False >> fput f x
  fget f = get >>= \v -> if v then Inl <$> fget f else Inr <$> fget f

instance (GBinary f, GBinary g) => GBinary (Prod f g) where
  fput f (Prod x y) = fput f x >> fput f y
  fget f = Prod <$> fget f <*> fget f

instance GBinary f => GBinary (Con f) where
  fput f (Con _ x) = fput f x
  fget f = Con "" <$> fget f

gput :: (GBinary (PF a), PFView a) => a -> Put
gput = fput gput . from

gget :: (GBinary (PF a), PFView a) => Get a
gget = to <$> fget gget

