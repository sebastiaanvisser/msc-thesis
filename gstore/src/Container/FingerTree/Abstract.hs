{-# LANGUAGE
    TemplateHaskell
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Container.FingerTree.Abstract where

import Control.Applicative
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
  Value  :: a                                            -> Tree a f (Nd Zero)
  Node1  :: f (Nd c)                                     -> Tree a f (Dg (S c))
  Node2  :: f (Nd c) -> f (Nd c)                         -> Tree a f (Nd (S c))
  Node3  :: f (Nd c) -> f (Nd c) -> f (Nd c)             -> Tree a f (Nd (S c))
  Node4  :: f (Nd c) -> f (Nd c) -> f (Nd c) -> f (Nd c) -> Tree a f (Dg (S c))

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

node1 :: Node a c -> Digit a (S c)
node1 a = HIn (Node1 a)

node2 :: Node a c -> Node a c -> Node a (S c)
node2 a b = HIn (Node2 a b)

node3 :: Node a c -> Node a c -> Node a c -> Node a (S c)
node3 a b c = HIn (Node3 a b c)

node4 :: Node a c -> Node a c -> Node a c -> Node a c -> Digit a (S c)
node4 a b c d = HIn (Node4 a b c d)

instance HFunctor (Tree a) where
  hfmap _ Empty           = Empty
  hfmap f (Single a)      = Single (f a)
  hfmap f (Deep a c b)    = Deep (f a) (f c) (f b)
  hfmap f (Digit a)       = Digit (f a)
  hfmap _ (Value a)       = Value a
  hfmap f (Node1 a)       = Node1 (f a)
  hfmap f (Node2 a b)     = Node2 (f a) (f b)
  hfmap f (Node3 a b c)   = Node3 (f a) (f b) (f c)
  hfmap f (Node4 a b c d) = Node4 (f a) (f b) (f c) (f d)

instance HFoldable (Tree a) where
  hfoldMap _ Empty           = mempty
  hfoldMap f (Single a)      = f a
  hfoldMap f (Deep a c b)    = mconcat [f a, f c, f b]
  hfoldMap f (Digit a)       = f a
  hfoldMap _ (Value _)       = mempty
  hfoldMap f (Node1 a)       = f a
  hfoldMap f (Node2 a b)     = mconcat [f a, f b]
  hfoldMap f (Node3 a b c)   = mconcat [f a, f b, f c]
  hfoldMap f (Node4 a b c d) = mconcat [f a, f b, f c, f d]

instance HTraversable (Tree a) where
  htraverse _ Empty           = pure Empty
  htraverse f (Single a)      = Single <$> f a
  htraverse f (Deep a c b)    = Deep <$> f a <*> f c <*> f b
  htraverse f (Digit a)       = Digit <$> f a
  htraverse _ (Value a)       = pure (Value a)
  htraverse f (Node1 a)       = Node1 <$> f a
  htraverse f (Node2 a b)     = Node2 <$> f a <*> f b
  htraverse f (Node3 a b c)   = Node3 <$> f a <*> f b <*> f c
  htraverse f (Node4 a b c d) = Node4 <$> f a <*> f b <*> f c <*> f d

infixr 5 <|

(<|) :: Node a c -> Spine a (S c) -> Spine a (S c)
a <| (HIn (Deep (            HIn (Node1 b      ))   m sf)) = deep   (digit (node2 a b    )) m                  sf
a <| (HIn (Deep (HIn (Digit (HIn (Node2 b c    )))) m sf)) = deep   (digit (node3 a b c  )) m                  sf
a <| (HIn (Deep (HIn (Digit (HIn (Node3 b c d  )))) m sf)) = deep          (node4 a b c d ) m                  sf
a <| (HIn (Deep (            HIn (Node4 b c d e))   m sf)) = deep   (digit (node2 a b    )) (node3 c d e <| m) sf
a <| (HIn (Single b                                     )) = deep   (node1 a)               empty_             b
a <| (HIn  Empty                                         ) = single (node1 a)
_ <| x                                                     = x

infixr 5 |>

(|>) :: Spine a (S c) -> Node a c -> Spine a (S c)
(HIn (Deep pr m (            HIn (Node1       b  )))) |> a = deep   pr m                  (digit (node2     b a))
(HIn (Deep pr m (HIn (Digit (HIn (Node2     c b)))))) |> a = deep   pr m                  (digit (node3   c b a))
(HIn (Deep pr m (HIn (Digit (HIn (Node3   d c b)))))) |> a = deep   pr m                  (       node4 d c b a)
(HIn (Deep pr m (HIn             (Node4 e d c b  )))) |> a = deep   pr (node3 e d c <| m) (digit (node2 b a    ))
(HIn (Single b                                     )) |> a = deep   b  empty_             (node1 a)
(HIn  Empty                                         ) |> a = single (node1 a)
x |> _                                                     = x

(|<|) :: [Value a] -> FingerTree a -> FingerTree a
(|<|) = flip (foldr (<|))

(|>|) :: [Value a] -> FingerTree a -> FingerTree a
(|>|) = flip (foldl (|>))

fromList :: [Value a] -> FingerTree a
fromList = (|<| empty_)

-------------------

test :: FingerTree Int
test = fromList (map value [30, 40, 40, 40, 40, 40, 20])

