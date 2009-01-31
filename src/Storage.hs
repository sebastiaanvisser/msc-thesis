{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Storage where

import Prelude hiding (lookup)

newtype Fix f = In {out :: f (Fix f)}

instance Show (f (Fix f)) => Show (Fix f) where
  show = ("<< " ++) . (++ " >>") . show . out

-- Tying the knots for recursive computations.

-- Modifications.
fixM :: ((Fix f -> Fix f) -> f (Fix f) -> f (Fix f)) -> Fix f -> Fix f
fixM f = In . f (fixM f) . out

-- Queries.
fixQ :: ((Fix f -> a) -> f (Fix f) -> a) -> Fix f -> a
fixQ f = f (fixQ f) . out

annQ :: (b -> c) -> ((t -> (t, [a])) -> (Fix f -> c) -> f (Fix f) -> b) -> Fix f -> b
annQ k f cont = f lift rec (out cont)
  where lift a = (a, [])
        rec = k . annQ k f

mytest :: Ord a => a -> Tree a b -> (Maybe b, [Char])
mytest key tree = annQ (\(a, b) -> (Nothing, [])) (\lift rec -> alookup lift (\a -> rec) key) tree

-------------------------------------------------------------------------------

-- Fixpoint parametrized domain.

data FTree a b f =
    Leaf
  | Branch a b f f
  deriving (Eq, Ord, Show, Read)

fempty :: FTree a b f
fempty = Leaf

finsert :: Ord a => (FTree a b f -> f) -> (a -> b -> f -> f) -> a -> b -> FTree a b f -> FTree a b f
finsert p f a b Leaf = Branch a b (p Leaf) (p Leaf)
finsert p f a b (Branch c d l r)
   | a > c     = Branch c d l (f a b r)
   | otherwise = Branch c d (f a b l) r

flookup :: (Ord a, Monad m) => (a -> f -> m b) -> a -> FTree a b f -> m b
flookup f a Leaf = fail "element not found"
flookup f a (Branch c d l r) =
  case a `compare` c of
    EQ -> return d
    LT -> f a l
    GT -> f a r

alookup
  :: (Ord a, Monad m)
  => (m b -> c)    -- lifter for query result
  -> (a -> f -> c) -- recursive alookup
  -> a             -- key to search for
  -> FTree a b f   -- tree to search in
  -> c             -- lifted query result
alookup p f a Leaf = p $ fail "element not found"
alookup p f a (Branch c d l r) =
  case a `compare` c of
    EQ -> p (return d)
    LT -> f a l
    GT -> f a r

-- mytest = alookup (\a -> (a :: Maybe Char, "ANN")) (\_ _ -> (undefined, "KIP")) (20 :: Int) fempty

-- Recursive domain.

type Tree a b = Fix (FTree a b)

empty :: Tree a b
empty = In fempty

insert :: Ord a => a -> b -> Tree a b -> Tree a b
insert a b = fixM (\f -> finsert In (\a b -> f) a b)

insertWith :: Ord a => (b -> a) -> b -> Tree a b -> Tree a b
insertWith f a = insert (f a) a

lookup :: (Ord a, Monad m) => a -> Tree a b -> m b
lookup a = fixQ (\f -> flookup (const f) a)

-------------------------------------------------------------------------------

type Title    = String
type Director = String
type Actor    = String

data Movie =
  Movie {
    title    :: Title
  , director :: Director
  , cast     :: [Actor]
  } deriving (Show, Read)

-- Compare movies by title.
instance Eq Movie where
  a == b = title a == title b

instance Ord Movie where
  a `compare` b = title a `compare` title b

type Database = Tree Title Movie

emptyMovie :: Movie
emptyMovie = Movie "" "" []

jurassicPark, anchorMan, zoolander :: Movie
jurassicPark = Movie "Jurassic Park" "Steven Spielberg" ["Sam Neill", "Richard Attenborough"]
anchorMan    = Movie "Anchorman"     "Adam McKay"       ["Will Ferell", "Christina Applegate", "Steve Carell"]
zoolander    = Movie "Zoolander"     "Geen idee"        ["Ben Stiller"]

insertWithTitle :: Movie -> Database -> Database
insertWithTitle = insertWith title

db :: Database
db = 
    insertWithTitle jurassicPark
  $ insertWithTitle anchorMan
  $ insertWithTitle zoolander
  $ empty

directorOf :: String -> String
directorOf movie = ("not found" `maybe` director) (lookup movie db)

