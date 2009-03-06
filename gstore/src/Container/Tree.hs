module Container.Tree where

import Control.Monad
import Data.Binary
import Generic.Persist
import Storage.Storage
import qualified Container.Abstract.Tree as F

-- A persistent Tree and a pointer to a persistent tree.

type Tree  a b = PFix  (F.Tree a b)
type TreeP a b = PFixP (F.Tree a b)

triplet
  :: (Show a, Show b, Binary a, Binary b)
  => a -> b -> a -> b -> a -> b
  -> Storage t (Persistent (Tree a b))
triplet a0 b0 a1 b1 a2 b2 = persistentP (F.triplet a0 b0 a1 b1 a2 b2)

lookup
  :: (Ord a, Monad m, Binary a, Binary b)
  => a -> TreeP a b -> Storage t (m b)
lookup a = persistentQ (F.lookup a)

count
  :: (Num c, Binary a, Binary b)
  => TreeP a b -> Storage t c
count = persistentQ F.count

