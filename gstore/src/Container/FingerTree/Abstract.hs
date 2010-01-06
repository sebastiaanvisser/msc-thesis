{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , TypeOperators
  , DeriveTraversable
  , RankNTypes
  , KindSignatures
  , GADTs
  , EmptyDataDecls
  , ScopedTypeVariables
 #-}
module Container.FingerTree.Abstract where

import Prelude hiding (foldr, foldl, sum)
import Control.Applicative
import Data.Foldable hiding (toList, sum)
import Data.Monoid
import Generics.Types

-- Peano numbers.

data Zero
data S c

-- Continue counting.

type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three

-- Nodes in a finger tree can represent a part of the spine or a part of the
-- fingers. The top level of the fingers are the digits which can have a branch
-- level of 1-4. The lower levels of the fingers are nodes which can have a 2-3
-- branch level. At the lowest level are the values.

data Sp c
data Dg c
data Nd c

data Tree (a :: *) (f :: * -> *) :: * -> * where
  Empty  ::                                                 Tree a f (Sp (S c))
  Single :: f (Dg c)                                     -> Tree a f (Sp c)
  Deep   :: f (Dg c) -> f (Sp (S c)) -> f (Dg c)         -> Tree a f (Sp c)
  Digit  :: f (Nd c)                                     -> Tree a f (Dg c)
  Digit1 :: f (Nd c)                                     -> Tree a f (Dg (S c))
  Digit4 :: f (Nd c) -> f (Nd c) -> f (Nd c) -> f (Nd c) -> Tree a f (Dg (S c))
  Value  :: a                                            -> Tree a f (Nd Zero)
  Node2  :: f (Nd c) -> f (Nd c)                         -> Tree a f (Nd (S c))
  Node3  :: f (Nd c) -> f (Nd c) -> f (Nd c)             -> Tree a f (Nd (S c))

-- Pretty names for common structures.

type Node       a c = HFixA (Tree a) (Nd c)
type Value      a   = Node a Zero
type Digit      a c = HFixA (Tree a) (Dg c)
type Spine      a c = HFixA (Tree a) (Sp c)
type FingerTree a   = HFixA (Tree a) (Sp One)

-- Bunch of smart constructors taking into the account the fixed point constructor HIn.

empty_ :: Spine a (S c)
empty_ = HIn Empty

single :: Digit a c -> Spine a c
single a = HIn (Single a)

deep :: Digit a c -> Spine a (S c) -> Digit a c -> Spine a c
deep a c b = HIn (Deep a c b)

digit :: Node a c -> Digit a c
digit a = HIn (Digit a)

value :: a -> Value a
value i = HIn (Value i)

node2 :: Node a c -> Node a c -> Node a (S c)
node2 a b = HIn (Node2 a b)

node3 :: Node a c -> Node a c -> Node a c -> Node a (S c)
node3 a b c = HIn (Node3 a b c)

digit1 :: Node a c -> Digit a (S c)
digit1 a = HIn (Digit1 a)

digit2 :: Node a c -> Node a c -> Digit a (S c)
digit2 a b = digit (node2 a b)

digit3 :: Node a c -> Node a c -> Node a c -> Digit a (S c)
digit3 a b c = digit (node3 a b c)

digit4 :: Node a c -> Node a c -> Node a c -> Node a c -> Digit a (S c)
digit4 a b c d = HIn (Digit4 a b c d)

-- Higher order functor, foldable and traversable instances.

instance HFunctor (Tree a) where
  hfmap _ Empty            = Empty
  hfmap f (Single a)       = Single (f a)
  hfmap f (Deep a c b)     = Deep (f a) (f c) (f b)
  hfmap f (Digit a)        = Digit (f a)
  hfmap _ (Value a)        = Value a
  hfmap f (Digit1 a)       = Digit1 (f a)
  hfmap f (Node2 a b)      = Node2 (f a) (f b)
  hfmap f (Node3 a b c)    = Node3 (f a) (f b) (f c)
  hfmap f (Digit4 a b c d) = Digit4 (f a) (f b) (f c) (f d)

instance HFoldable (Tree a) where
  hfoldMap _ Empty            = mempty
  hfoldMap f (Single a)       = f a
  hfoldMap f (Deep a c b)     = mconcat [f a, f c, f b]
  hfoldMap f (Digit a)        = f a
  hfoldMap _ (Value _)        = mempty
  hfoldMap f (Digit1 a)       = f a
  hfoldMap f (Node2 a b)      = mconcat [f a, f b]
  hfoldMap f (Node3 a b c)    = mconcat [f a, f b, f c]
  hfoldMap f (Digit4 a b c d) = mconcat [f a, f b, f c, f d]

instance HTraversable (Tree a) where
  htraverse _ Empty            = C (pure Empty)
  htraverse f (Single a)       = C (Single <$> unC (f a))
  htraverse f (Deep a c b)     = C (Deep <$> unC (f a) <*> unC (f c) <*> unC (f b))
  htraverse f (Digit a)        = C (Digit <$> unC (f a))
  htraverse _ (Value a)        = C (pure (Value a))
  htraverse f (Digit1 a)       = C (Digit1 <$> unC (f a))
  htraverse f (Node2 a b)      = C (Node2  <$> unC (f a) <*> unC (f b))
  htraverse f (Node3 a b c)    = C (Node3  <$> unC (f a) <*> unC (f b) <*> unC (f c))
  htraverse f (Digit4 a b c d) = C (Digit4 <$> unC (f a) <*> unC (f b) <*> unC (f c) <*> unC (f d))

-- Left and right biased insertions.

infixr 5 <|

(<|) :: Node a c -> Spine a (S c) -> Spine a (S c)
a <| (HIn (Deep (            HIn (Digit1 b      ))   m sf)) = deep   (digit2 a b    ) m                  sf
a <| (HIn (Deep (HIn (Digit (HIn (Node2  b c    )))) m sf)) = deep   (digit3 a b c  ) m                  sf
a <| (HIn (Deep (HIn (Digit (HIn (Node3  b c d  )))) m sf)) = deep   (digit4 a b c d) m                  sf
a <| (HIn (Deep (            HIn (Digit4 b c d e))   m sf)) = deep   (digit2 a b    ) (node3 c d e <| m) sf
a <| (HIn (Single b                                      )) = deep   (digit1 a)       empty_             b
a <| (HIn  Empty                                          ) = single (digit1 a)
_ <| x                                                      = x

infixr 5 |>

(|>) :: Spine a (S c) -> Node a c -> Spine a (S c)
(HIn (Deep pr m (            HIn (Digit1       b  )))) |> a = deep   pr m                  (digit2     b a)
(HIn (Deep pr m (HIn (Digit (HIn (Node2      c b)))))) |> a = deep   pr m                  (digit3   c b a)
(HIn (Deep pr m (HIn (Digit (HIn (Node3    d c b)))))) |> a = deep   pr m                  (digit4 d c b a)
(HIn (Deep pr m (HIn             (Digit4 e d c b  )))) |> a = deep   pr (node3 e d c <| m) (digit2     b a)
(HIn (Single b                                      )) |> a = deep   b  empty_             (digit1       a)
(HIn  Empty                                          ) |> a = single                       (digit1       a)
x |> _                                                      = x

(|<|) :: Foldable f => f (Value a) -> FingerTree a -> FingerTree a
(|<|) = flip (foldr (<|))

(|>|) :: Foldable f => f (Value a) -> FingerTree a -> FingerTree a
(|>|) = flip (foldl (|>))

fromList :: [Value a] -> FingerTree a
fromList = (|<| empty_)

getValue :: Tree a f ix -> [a]
getValue (Value a) = pure a
getValue _         = mempty

toList :: HFixA (Tree a) c -> [a]
toList = foldm getValue

-------------------

test :: FingerTree Int
test = fromList (map value [3, 12, 44, 5, 2, 100, 20])

sumAlg :: Tree Int (K Int) ix -> Int
sumAlg (Empty         ) = 0
sumAlg (Single a      ) = unK a
sumAlg (Value  a      ) = a
sumAlg (Digit  a      ) = unK a
sumAlg (Digit1 a      ) = unK a
sumAlg (Digit4 a b c d) = unK a + unK b + unK c + unK d
sumAlg (Node2  a b    ) = unK a + unK b
sumAlg (Node3  a b c  ) = unK a + unK b + unK c
sumAlg (Deep   a b c  ) = unK a + unK b + unK c

sum :: HFixA (Tree Int) ix -> Int
sum = unK . hfold (K . sumAlg)

containsAlg :: Eq a => a -> Tree a (K Bool) ix -> Bool
containsAlg _ (Empty         ) = False
containsAlg _ (Single a      ) = unK a
containsAlg v (Value  a      ) = a == v
containsAlg _ (Digit  a      ) = unK a
containsAlg _ (Digit1 a      ) = unK a
containsAlg _ (Digit4 a b c d) = unK a || unK b || unK c || unK d
containsAlg _ (Node2  a b    ) = unK a || unK b
containsAlg _ (Node3  a b c  ) = unK a || unK b || unK c
containsAlg _ (Deep   a b c  ) = unK a || unK b || unK c

contains :: Eq a => a -> HFixA (Tree a) ix -> Bool
contains v = unK . hfold (K . containsAlg v)







-- ******************************************************** TEST **********************




data PTree a = PLeaf a | PNode (PTree (a,a))

data HPTree f a = HPLeaf a | HPNode (f (a,a))

-- type PTree a = HFixA HPTree a

instance HFunctor HPTree where
  hfmap _ (HPLeaf a) = HPLeaf a
  hfmap f (HPNode a) = HPNode (f a)

hfoldPTree
  :: (forall a. a       -> f a)
  -> (forall a. f (a,a) -> f a)
  -> PTree b -> f b
hfoldPTree f _ (PLeaf x)  = f x
hfoldPTree f g (PNode xs) = g (hfoldPTree f g xs)


-- type Blaat = Ran (K Int) (K Int)

myPTree :: PTree Int
myPTree =
  (PNode . PNode . PNode . PLeaf)
  (((1, 2), (3, 4)), ((5, 6), (7, 8)))

psum :: PTree Int -> Int
psum = aux id
  where aux :: (a -> Int) -> PTree a -> Int
        aux f (PLeaf x)  = f x
        aux f (PNode xs) = aux (\(a, b) -> f a + f b) xs

bliep :: [Int]
bliep = hfoldPTree x y myPTree
  where x = (:[])
        y = Prelude.concatMap (\(a, b) -> [b, a])

alg :: HPTree (Ran String String) a -> Ran String String a
alg (HPLeaf a) = Ran (\s -> s a)
alg (HPNode r) = Ran (\s -> unRan r (\(a, b) -> "(" ++ s a ++ ", " ++ s b ++ ")"))

