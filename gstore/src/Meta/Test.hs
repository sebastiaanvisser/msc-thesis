{-# LANGUAGE TemplateHaskell #-}
module Test where

import Prelude hiding (lookup)
import Meta

$(convert
 [d|

  data Tree a b =
      Leaf
    | Branch {
        key   :: a
      , val   :: b
      , left  :: Tree a b
      , right :: Tree a b
      }
    deriving (Eq, Ord, Show, Read)

  type AA aa = aa

  lookup :: (Monad m, Ord a) => a -> Tree a b -> m b
  lookup _ Leaf = fail "element not found"
  lookup a (Branch c d l r) =
    case a `compare` c of
      EQ -> return d
      LT -> lookup a l
      GT -> lookup a r

  count :: Num c => Tree a b -> c
  count Leaf = 0
  count t    = 1 + count (left t) + count (right t)

  depth :: (Ord c, Num c) => Tree a b -> c
  depth Leaf = 0
  depth t    = 1 + depth (left t) `max` depth (right t)

  |])

depth' :: (Ord c, Num c, Monad m) => Query (Tree a b) f m c
depth' _ Leaf = return 0
depth' f t =
  do a <- f (left  t)
     b <- f (right t)
     return (1 + max a b)

{-

data Tree a b f =
    Leaf
  | Branch {key :: a, val :: b, left :: f, right :: f}
  deriving (Eq, Ord, Show, Read)

count :: (Num c, Monad m) => (f -> m c) -> Tree a b f -> m c
count _ Leaf = return 0
count f t =
  do a <- f (left  t)
     b <- f (right t)
     return (1 + a + b)

-}
