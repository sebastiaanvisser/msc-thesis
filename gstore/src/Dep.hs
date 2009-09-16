
import Data.Foldable
-- import Generics.Regular.Base
-- import Data.Binary

-- deriving instance Show f           => Show (I f)
-- deriving instance Show (f (Fix f)) => Show (Fix f)

-- instance Binary (f (Fix f)) => Binary (Fix f) where
--   put = put . out
--   get = In `fmap` get

-- Function composition at the type level.
-- Compose :: (* -> *) -> (* -> *) -> * -> *
infixr :.:
newtype (f :.: g) a = O { unO :: f (g a) }
  deriving Show

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f = O . fmap (fmap f) . unO

instance (Foldable f, Foldable g) => Foldable (f :.: g) where
  foldMap f = foldMap (foldMap f) . unO


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





-- instance Binary (f (g a)) => Binary ((f :. g) a) where
--   get = O `fmap` get
--   put = put . unO

