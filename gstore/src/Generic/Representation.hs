{-# LANGUAGE
    StandaloneDeriving
  , TypeFamilies
  , FlexibleContexts
  , UndecidableInstances
  , TypeOperators
  #-}
module Generic.Representation where

import Data.Binary

-- Generic representation.

data Id       r = Id   { unId :: r }                        deriving Show
data K    a   r = K    { unK :: a }                         deriving Show
data Unit     r = Unit                                      deriving Show
data Sum  f g r = Inl  { left :: f r} | Inr { right :: g r} deriving Show
data Prod f g r = Prod { fist :: f r, second :: g r}        deriving Show
data Con  f   r = Con  { conName :: String, unCon :: f r}   deriving Show
data F    f   r = F    { unF :: f r}                        deriving Show

-- Fixed-point constructor.

newtype Fix f = In {out :: f (Fix f) }

-- deriving instance Show (f (Fix f)) => Show (Fix f)

instance Show (f (Fix f)) => Show (Fix f) where
  show = ("[| " ++) . (++ " |]") . show . out

instance Binary (f (Fix f)) => Binary (Fix f) where
  get = In `fmap` get
  put = put . out

-- Generic recursive view on data types.

-- class TyView a where
--   name  :: a -> String
--   ctors :: a -> [String]

class PFView a where
  type PF a :: * -> *
  from      :: a -> PF a a
  to        :: PF a a -> a

-- Function composition at the type level.
-- Compose :: (* -> *) -> (* -> *) -> * -> *
infixr :.
newtype (f :. g) a = C { unC :: f (g a) }
  deriving Show

type AnnFix  f a =    Fix (f :. a)
type AnnFixF f a = a (Fix (f :. a))

