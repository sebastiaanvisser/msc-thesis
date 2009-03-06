{-# LANGUAGE
    FlexibleContexts
  , UndecidableInstances
  , TypeOperators
 #-}
module Generic.Annotate where

import Data.Binary
import Generic.Core

-- Function composition at the type level.
-- Compose :: (* -> *) -> (* -> *) -> * -> *
infixr :.
newtype (f :. g) a = C { unC :: f (g a) }
  deriving Show

instance Binary (f (g a)) => Binary ((f :. g) a) where
  get = C `fmap` get
  put = put . unC

-- Annotated fix points compose an annotation with the container.
type AnnFix f ann = Fix (f :. ann)

-- Queries.
fixQ :: ((Fix f -> a) -> f (Fix f) -> a) -> Fix f -> a
fixQ q = q (fixQ q) . out

-- Annotated queries.
annFixQ
  :: (Fix f -> t)                           -- Container indexed lift function.
  -> (Fix f -> b -> c)                      -- Container indexed post processor.
  -> (t -> (Fix f -> c) -> f (Fix f) -> b)  -- Real query function.
  -> Fix f                                  -- Container to query in.
  -> b                                      -- Annotate query result.
annFixQ l p q c = q (l c) (p c . annFixQ l p q) (out c)

-- Annotated query that tracks all nodes it traverses.
traceQ
  :: ((t -> (t, [a])) -> (Fix f -> (b, [a])) -> f (Fix f) -> (b, [a]))
  -> (f (Fix f) -> a)
  -> Fix f
  -> (b, [a])
traceQ q s = annFixQ lift post q 
  where lift c a      = (a, [s (out c)])
        post c (a, b) = (a,  s (out c) : b)

-- Annotated queries in some monad.
monadicQ
  :: Monad m        -- Monad we live in.
  => (t -> m a)     -- Lifted unwrap function.
  ->   ((c -> m c)  -- Lift function.
     -> (t -> m b)  -- Post processor.
     -> a           -- Recursive container to query in.
     -> m b)        -- Recursive lifted query result.
  -> t              -- Container to query in.
  -> m b            -- Lifted query result.
monadicQ p q c = p c >>= q return (monadicQ p q)

-- Annotated queries in the IO monad.
ioQ
  :: (t -> IO a)
  -> ((c -> IO c) -> (t -> IO b) -> a -> IO b)
  -> t
  -> IO b
ioQ = monadicQ

-- Annotated queries on fix-point containers inside the IO monad.
ioFixQ
  :: (f (Fix f) -> IO a)
  -> ((c -> IO c) -> (Fix f -> IO b) -> a -> IO b)
  -> Fix f
  -> IO b
ioFixQ p = ioQ (p . out)

-------------------------------------------------------------------------------

-- Producers.
fixP :: (Fix f -> f (Fix f)) -> Fix f
fixP p = In (p (fixP p))

-- Modifications.
fixM :: ((Fix f -> Fix f) -> f (Fix f) -> f (Fix f)) -> Fix f -> Fix f
fixM m = In . m (fixM m) . out

