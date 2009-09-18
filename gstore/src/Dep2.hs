{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Container.Tree.PersistentMorphism where

import Annotation.Debug
import Annotation.Persistent
import Data.Binary
import Generics.Aspect
import Generics.Representation
import Prelude hiding (replicate, lookup)
import Storage.FileStorage
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Morphism as M

anaM
  :: (Aspect f (AnnFix (F.Tree a b) f) m)
  => M.Anamorphism a b s
  -> s -> m (AnnFixF (F.Tree a b) f)
anaM alg s =
  case alg s of
    Nothing -> (produce . In . C) F.Leaf
    Just (a, b, u, v) ->
      do l <- anaM alg u
         r <- anaM alg v
         (produce . In . C) (F.Branch a b l r)

cataM
  :: (Aspect f (AnnFix (F.Tree a b) f) m)
  => M.Catamorphism a b c
  -> AnnFix (F.Tree a b) f
  -> m c
cataM alg@(x, _) (In (C  F.Leaf)) = return x
cataM alg@(_, y) (In (C (F.Branch a b l r))) =
  do lt <- query l >>= cataM alg
     rt <- query r >>= cataM alg
     return (y a b lt rt)

endoparaM
  :: (Aspect f (AnnFix (F.Tree a b) f) m)
  => M.Endoparamorphism a b
  -> AnnFix  (F.Tree a b) f
  -> m (AnnFixF (F.Tree a b) f)
endoparaM alg@(x, _) (In (C  F.Leaf)) = let xx = x (In (C F.Leaf)) (In (C F.Leaf)) in return xx
-- endoparaM alg@(_, y) (In (C (F.Branch a b l r))) =
--   do lt <- query l >>= endoparaM alg
--      rt <- query r >>= endoparaM alg
--      return (y a b lt rt)












type TreeAspects = Pointer :. Debug :. Id

class (Show a, Binary a) => TreeClass a

type Tree  a b = AnnFix  (F.Tree a b) TreeAspects
type TreeP a b = AnnFixF (F.Tree a b) TreeAspects





replicate
  :: (TreeClass a, TreeClass b, Integral i)
  => a -> b -> i -> Storage t (TreeP a b)
replicate a b = anaM (M.replicate a b)

fromList
  :: (TreeClass a, TreeClass b)
  => [(a, b)]
  -> Storage t (TreeP a b)
fromList = anaM M.fromList

lookup
  :: (TreeClass a, TreeClass b, Monad m, Ord a)
  => a -> Tree a b -> Storage t (m b)
lookup a = cataM (M.lookup a)

count
  :: (TreeClass a, TreeClass b, Num i)
  => Tree a b -> Storage t i
count = cataM M.count

depth
  :: (TreeClass a, TreeClass b, Num i, Ord i)
  => Tree a b -> Storage t i
depth = cataM M.depth

