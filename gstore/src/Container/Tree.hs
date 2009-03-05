module Container.Tree where

import Data.Binary
import Control.Monad
import Generic.Annotate
import Generic.Core hiding (left, right)
import Heap.Storage
import Generic.Persist

import qualified Container.Abstract.Tree as F

-- A persistent Tree and a pointer to a persistent tree.
type Tree  a b = PFix  (F.Tree a b)
type TreeP a b = PFixP (F.Tree a b)

testTreePointer :: TreeP a b
testTreePointer = P 2134234

testPTree :: Tree Char Char
testPTree = In $ C $ F.Branch 'a' 'b' testTreePointer testTreePointer

tripletP
  :: (Show a, Show b, Binary a, Binary b)
  => a -> b -> a -> b -> a -> b
  -> Storage t (Persistent (Tree a b))
tripletP a0 b0 a1 b1 a2 b2 = persistentP $ F.tripletA a0 b0 a1 b1 a2 b2

-- countP
--   :: (Num c, Binary a, Binary b)
--   => TreeP a b -> Storage t c

lookupP
  :: (Ord a, Monad m, Binary a, Binary b)
  => a -> TreeP a b -> Storage t (m b)
lookupP a t = query t >>= worker
  where worker = F.lookupA a (\v -> query v >>= worker)

countP
  :: (Num c, Binary a, Binary b)
  => TreeP a b -> Storage t c
countP t = query t >>= worker
  where worker = F.countA (\v -> query v >>= worker)

