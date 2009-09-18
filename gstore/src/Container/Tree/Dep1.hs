{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , TypeOperators
  , TypeFamilies
  , TemplateHaskell
  , EmptyDataDecls
  #-}
module Container.Tree.Dep1 where

import Prelude hiding (replicate, lookup)
import Generics.Regular
-- import qualified Container.Tree.Abstract as F
import Generics.Regular
import Generics.Regular.Binary
import Generics.Regular.TH

data Tree a b =
    Leaf
  | Branch a b (Tree a b) (Tree a b)
  deriving (Show, Eq)

$(deriveAll ''Tree "PFTree")
type instance PF (Tree a b) = PFTree a b

count :: Num i => Algebra (Tree a b) i
count = 0 & (\_ _ l r -> 1 + l + r)

depth :: (Num i, Ord i) => Algebra (Tree a b) i
depth = 0 & (\_ _ l r -> 1 + max l r)

lookup :: (Monad m, Ord a) => a -> Algebra (Tree a b) (m b)
lookup a = (fail "element not found", op)
  where
    op c d l r =
      case a `compare` c of
        EQ -> return d
        LT -> r
        GT -> l

type Update f = PAlg (PF f) (Fix (CoAlgT (PF f)))
















{-
insert :: Ord a => a -> b -> Update (Tree a b)
insert a b =
  let mkT k l m = In . CoAlgT . Just $ (k & l & m) in
  ( \g                 -> mkT a b (g , undefined) --  (g, g)
  , \c d (l, x) (r, y) -> mkT c d (undefined, undefined) -- $ if a > c then (l, y) else (x, r)
  )

replicate :: Integral i => a -> b -> CoAlgebra (Tree a b) i
replicate a b 0 = Nothing
replicate a b i = let j = (i - 1) `div` 2 in Just (a & b & j & i - 1 - j)

singleton :: a -> b -> CoAlgebra (Tree a b) Bool
singleton _ _ False = Nothing
singleton a b True  = Just (a & b & False & False)

fromList :: CoAlgebra (Tree a b) [(a, b)]
fromList []          = Nothing
fromList ((a, b):ab) = Just (a & b & splitAt (length ab `div` 2) ab)




runFixCoAlgT :: (Unfold (PF f), Regular f) => Fix (CoAlgT (PF f)) -> f
runFixCoAlgT m = unfold ((const . unCoAlgT . out) m) m 

doiets :: Tree Bool Char
doiets = runFixCoAlgT insTest


insTest :: Fix (CoAlgT (PF (Tree Bool Char)))
insTest = pfold (insert False 'b') myTest

myTest :: Tree Bool Char
myTest = unfold (replicate True 'a') 4
 
bla :: Tree String Int
bla = unfold fromList
  [ ("hallo", 20)
  , ("kip", 100)
  , ("aap", 25)
  , ("ding", 20)
  , ("dino", 13)
  ]
-}

