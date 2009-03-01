{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Container.Tree where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Generic
import Generic.Annotate
import Generic.Core hiding (left, right)
import Heap.FileHeap
import Generic.Persist
import Prelude hiding (lookup, read)

-- Binary tree parametrized by key type, value type and recursive points.

data FTree a b f =
    Leaf
  | Branch {key :: a, val :: b, left :: f, right :: f}
  deriving (Eq, Ord, Show, Read)

-- Generic pattern functor view.

instance PFView (FTree a b f) where
  type PF (FTree a b f) = Sum Unit (Prod (Prod (K a) (K b)) (Prod (K f) (K f)))

  from Leaf             = Inl Unit
  from (Branch a b f g) = Inr (Prod (Prod (K a) (K b)) (Prod (K f) (K g)))

  to (Inl Unit)                                         = Leaf
  to (Inr (Prod (Prod (K a) (K b)) (Prod (K f) (K g)))) = Branch a b f g

-- Generically derived binary instance.

instance (Binary a, Binary b, Binary f) => Binary (FTree a b f) where
  put = gput
  get = gget

-- Basic functions.

fempty :: FTree a b f
fempty = Leaf

fsingleton :: a -> b -> (FTree a b f -> f) -> FTree a b f
fsingleton a b p = Branch a b (p Leaf) (p Leaf)

ftriplet :: a -> b -> a -> b -> a -> b -> (FTree a b f -> f) -> FTree a b f
ftriplet a0 b0 a1 b1 a2 b2 p =
  Branch a1 b1
    (p (Branch a0 b0 (p Leaf) (p Leaf)))
    (p (Branch a2 b2 (p Leaf) (p Leaf)))

tripletA
  :: Monad m
  => a -> b -> a -> b -> a -> b
  -> (FTree a b f -> m f) -> m (FTree a b f)
tripletA a0 b0 a1 b1 a2 b2 p =
  do leaf  <- p $ Leaf
     left  <- p $ Branch a0 b0 leaf leaf
     right <- p $ Branch a2 b2 leaf leaf
     return $ Branch a1 b1 left right

finsert :: Ord a => a -> b -> (FTree a b f -> f) -> (f -> f) -> FTree a b f -> FTree a b f
finsert a b p _ Leaf = Branch a b (p Leaf) (p Leaf)
finsert a b _ f (Branch c d l r)
   | a > c     = Branch c d l (f r)
   | otherwise = Branch c d (f l) r

fromList :: Ord a => [(a, b)] -> Tree a b
fromList = foldr (uncurry insert) emptyTree

fLookup
  :: (Ord a, Monad m)
  => a
  -> (f -> m b)
  -> FTree a b f
  -> m b
fLookup _ _ Leaf = fail "element not found"
fLookup a f (Branch c d l r) =
  case a `compare` c of
    EQ -> return d
    LT -> f l
    GT -> f r

lookupA
  :: (Ord a, Monad m)
  => a            -- key to search for
  -> (m b -> c)   -- lifter for query result
  -> (f -> c)     -- recursive annotated lookup
  -> FTree a b f  -- tree to search in
  -> c            -- lifted query result
lookupA _ p _ Leaf = p $ fail "element not found"
lookupA a p f (Branch c d l r) =
  case a `compare` c of
    EQ -> p (return d)
    LT -> f l
    GT -> f r

countA :: (Num c, Monad m) => (c -> m c) -> (f -> m c) -> FTree a b f -> m c
countA p _ Leaf = p 0
countA p f t =
  do a <- f (left  t)
     b <- f (right t)
     p (a + b + 1)

---------------------- KNOTS TIED

newtype Tree a b = Tree { ftree :: Fix (FTree a b) }
  deriving Show

withFTree :: (Fix (FTree a b) -> Fix (FTree c d)) -> Tree a b -> Tree c d
withFTree f = Tree . f . ftree

emptyTree :: Tree a b
emptyTree = Tree $ In fempty

singleton :: a -> b -> Tree a b
singleton a b = Tree . In $ fsingleton a b In

triplet :: a -> b -> a -> b -> a -> b -> Tree a b
triplet a0 b0 a1 b1 a2 b2 = Tree . In $ ftriplet a0 b0 a1 b1 a2 b2 In

insert :: Ord a => a -> b -> Tree a b -> Tree a b
insert a b = withFTree $ fixM $ finsert a b In

-------------------------- ANNOTATED TESTS

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

-------------------------- PERSISTENT

newtype PTree a b = PTree { ptree :: FTree a b Int }
  deriving Show

treeDecoder :: (Binary a, Binary b, Binary f) => Int -> Heap (FTree a b f)
treeDecoder = fmap decode . read

tripletP :: (Binary a, Binary b) => a -> b -> a -> b -> a -> b -> Heap Int
tripletP a0 b0 a1 b1 a2 b2 = persistentP (tripletA a0 b0 a1 b1 a2 b2)

lookupP :: (Show b, Binary a, Binary b, Ord a) => a -> Int -> Heap (Maybe b)
lookupP = monadicQ treeDecoder . lookupA

-- countP :: (Binary a, Binary b, Binary f) => FTree a b f -> Int -> Heap Int
-- countP _ = monadicQ (\x -> (treeDecoder x) :: Heap (FTree a b f)) countA

