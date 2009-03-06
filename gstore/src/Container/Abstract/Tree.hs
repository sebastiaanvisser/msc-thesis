{-# LANGUAGE TypeFamilies #-}
module Container.Abstract.Tree where

import Data.Binary
import Data.Binary.Generic
import Generic.Core hiding (left, right)

-- Binary tree parametrized by key type, value type and recursive points.

data Tree a b f =
    Leaf
  | Branch {key :: a, val :: b, left :: f, right :: f}
  deriving (Eq, Ord, Show, Read)

-- Generic pattern functor view.

instance PFView (Tree a b f) where
  type PF (Tree a b f) = Sum Unit (Prod (Prod (K a) (K b)) (Prod (K f) (K f)))

  from Leaf             = Inl Unit
  from (Branch a b f g) = Inr (Prod (Prod (K a) (K b)) (Prod (K f) (K g)))

  to (Inl Unit)                                         = Leaf
  to (Inr (Prod (Prod (K a) (K b)) (Prod (K f) (K g)))) = Branch a b f g

-- Generically derived binary instance.

instance (Binary a, Binary b, Binary f) => Binary (Tree a b f) where
  put = gput
  get = gget

-- Basic functions.


empty
  :: Monad m
  => (Tree a b f -> m f)
  -> m f
empty p = p Leaf

singleton
  :: Monad m
  => a -> b
  -> (Tree a b f -> m f)
  -> m f
singleton a b p =
  do l0 <- p Leaf
     l1 <- p Leaf
     p (Branch a b l0 l1)

triplet
  :: Monad m
  => a -> b -> a -> b -> a -> b
  -> (Tree a b f -> m f)
  -> m f
triplet a0 b0 a1 b1 a2 b2 p =
  do l0 <- p Leaf
     l1 <- p Leaf
     l2 <- p Leaf
     l3 <- p Leaf
     lt <- p (Branch a0 b0 l0 l1)
     rt <- p (Branch a2 b2 l2 l3)
     p (Branch a1 b1 lt rt)

lookup
  :: (Monad m, Monad n, Ord a)
  => a
  -> (f -> m (n b))
  -> Tree a b f
  -> m (n b)
lookup _ _ Leaf = return $ fail "element not found"
lookup a f (Branch c d l r) =
  case a `compare` c of
    EQ -> return (return d)
    LT -> f l
    GT -> f r

count :: (Num c, Monad m) => (f -> m c) -> Tree a b f -> m c
count _ Leaf = return 0
count f t =
  do a <- f (left  t)
     b <- f (right t)
     return (a + b + 1)

insert
  :: (Monad m, Ord a)
  => a -> b
  -> (Tree a b f -> m f)
  -> (f -> m f)
  -> Tree a b f -> m f
insert a b p _ Leaf = singleton a b p
insert a _ p f (Branch c d l r)
  | a > c     = f r >>= p .       Branch c d  l
  | otherwise = f l >>= p . flip (Branch c d) r

---------------------- KNOTS TIED

{-
newtype Tree a b = Tree { ftree :: Fix (Tree a b) }
  deriving Show

withFTree :: (Fix (Tree a b) -> Fix (Tree c d)) -> Tree a b -> Tree c d
withFTree f = Tree . f . ftree

emptyTree :: Tree a b
emptyTree = Tree $ In fempty

singleton :: a -> b -> Tree a b
singleton a b = Tree . In $ fsingleton a b In

triplet :: a -> b -> a -> b -> a -> b -> Tree a b
triplet a0 b0 a1 b1 a2 b2 = Tree . In $ ftriplet a0 b0 a1 b1 a2 b2 In

insert :: Ord a => a -> b -> Tree a b -> Tree a b
insert a b = withFTree $ fixM $ finsert a b In

fromList :: Ord a => [(a, b)] -> Tree a b
fromList = foldr (uncurry insert) emptyTree

-}

-------------------------- ANNOTATED TESTS

{-
insertWith :: Ord a => (b -> a) -> b -> Tree a b -> Tree a b
insertWith f a = insert (f a) a

--

lookup :: (Ord a, Monad m) => a -> Tree a b -> m b
lookup a = fixQ (fLookup a) . ftree

traceLookup :: (Ord a, Monad m) => a -> Tree a b -> (m b, [a])
traceLookup a = traceQ (lookupA a) key . ftree

ioFixLookup :: (Ord a, Monad m, Show a, Show b) => a -> Tree a b -> IO (m b)
ioFixLookup a = ioFixQ proc (lookupA a) . ftree
  where proc c = print c >> return c
-}

