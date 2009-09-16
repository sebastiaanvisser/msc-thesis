module Generics.Aspect where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad
import Generics.Representation
-- import Generics.Regular.Base

class Monad m => Aspect a f c m where
  query   :: a f c -> m (f c)
  produce :: f c -> m (a f c)
  modify  :: (f c -> m (a f c), a f c -> m (f c))
  modify = (produce, query)

instance Monad m => Aspect Id f c m where
  query   = return . unId
  produce = return . Id

-- Combine two different aspects into one.

-- instance (Monad m, Aspect a (b f) m, Aspect b f m) => Aspect (a :. b) f m where
--   query   = (query >=> query) . unO
--   produce = produce >=> produce >=> return . O
--   modify  = ( fst modify >=> fst modify >=> return . O
--             ,(snd modify >=> snd modify) . unO)

-- class Unwrap a where
--   type UW a :: *
--   unwrap :: a -> UW a

-- instance Unwrap f => Unwrap (I f) where
--   type UW (I f) = UW f
--   unwrap = unwrap . unI

-- instance Unwrap (a (b f)) => Unwrap ((a :. b) f) where
--    type UW ((a :. b) f) = UW (a (b f))
--    unwrap = unwrap . unO

