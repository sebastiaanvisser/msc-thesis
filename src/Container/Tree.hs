module Container.Tree where

import Prelude hiding (lookup)
import Generic.Annotate

-- Fixpoint parametrized domain.

data FTree a b f =
    Leaf
  | Branch {key :: a, val :: b, left :: f, right :: f}
  deriving (Eq, Ord, Show, Read)

fempty :: FTree a b f
fempty = Leaf

finsert :: Ord a => (FTree a b f -> f) -> (a -> b -> f -> f) -> a -> b -> FTree a b f -> FTree a b f
finsert p _ a b Leaf = Branch a b (p Leaf) (p Leaf)
finsert _ f a b (Branch c d l r)
   | a > c     = Branch c d l (f a b r)
   | otherwise = Branch c d (f a b l) r

fromList :: Ord a => [(a, b)] -> Tree a b
fromList = foldr (uncurry insert) empty

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

-- Real domain.

newtype Tree a b = Tree { ftree :: Fix (FTree a b) }
  deriving Show

withFTree :: (Fix (FTree a b) -> Fix (FTree c d)) -> Tree a b -> Tree c d
withFTree f = Tree . f . ftree

empty :: Tree a b
empty = Tree (In fempty)

-- todo fix insert
insert :: Ord a => a -> b -> Tree a b -> Tree a b
insert a b = withFTree $ fixM (\f -> finsert In (\_ _ -> f) a b)

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

