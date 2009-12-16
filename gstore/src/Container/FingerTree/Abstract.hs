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

-- Nodes in a finger tree either represent a part of the spine or a part of the
-- value fingers.

data Sp c
data Dg c
data Nd c

data Finger (a :: *) (f :: * -> *) :: * -> * where
  Empty  ::                                                 Finger a f (Sp c)
  Single :: f (Dg c) ->                                     Finger a f (Sp c)
  Deep   :: f (Dg c) -> f (Sp (S c)) -> f (Dg c) ->         Finger a f (Sp c)

  Digit  :: f (Nd c) ->                                     Finger a f (Dg c)
  Value  :: a ->                                            Finger a f (Nd Zero)
  Node1  :: f (Nd c) ->                                     Finger a f (Dg (S c))
  Node2  :: f (Nd c) -> f (Nd c) ->                         Finger a f (Nd (S c))
  Node3  :: f (Nd c) -> f (Nd c) -> f (Nd c) ->             Finger a f (Nd (S c))
  Node4  :: f (Nd c) -> f (Nd c) -> f (Nd c) -> f (Nd c) -> Finger a f (Dg (S c))

type Root a = HFixA (Finger a) (Sp Zero)

-- Bunch of smart constructors taking into the account the fixed point constructor HIn.

empty_ :: HFixA (Finger a) (Sp c)
empty_ = HIn Empty

single :: HFixA (Finger a) (Dg c) -> HFixA (Finger a) (Sp c)
single a = HIn (Single a)

deep :: HFixA (Finger a) (Dg c) -> HFixA (Finger a) (Sp (S c)) -> HFixA (Finger a) (Dg c) -> HFixA (Finger a) (Sp c)
deep a c b = HIn (Deep a c b)

digit :: HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Dg c)
digit a = HIn (Digit a)

value :: a -> HFixA (Finger a) (Nd Zero)
value i = HIn (Value i)

node1 :: HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Dg (S c))
node1 a = HIn (Node1 a)

node2 :: HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Nd (S c))
node2 a b = HIn (Node2 a b)

node3 :: HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Nd (S c))
node3 a b c = HIn (Node3 a b c)

node4 :: HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Dg (S c))
node4 a b c d = HIn (Node4 a b c d)

infixr 5 <|

(<|) :: HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Sp (S c)) -> HFixA (Finger a) (Sp (S c))
a <| (HIn (Deep (            HIn (Node1 b      ))   m sf)) = deep   (digit (node2 a b    )) m                  sf
a <| (HIn (Deep (HIn (Digit (HIn (Node2 b c    )))) m sf)) = deep   (digit (node3 a b c  )) m                  sf
a <| (HIn (Deep (HIn (Digit (HIn (Node3 b c d  )))) m sf)) = deep          (node4 a b c d ) m                  sf
a <| (HIn (Deep (            HIn (Node4 b c d e))   m sf)) = deep   (digit (node2 a b    )) (node3 c d e <| m) sf
a <| (HIn (Single b                                     )) = deep   (node1 a)               empty_             b
a <| (HIn  Empty                                         ) = single (node1 a)
_ <| x = x

infixr 5 |>

(|>) :: HFixA (Finger a) (Sp (S c)) -> HFixA (Finger a) (Nd c) -> HFixA (Finger a) (Sp (S c))
(HIn (Deep pr m (            HIn (Node1       b  )))) |> a = deep pr m                  (digit (node2     b a))
(HIn (Deep pr m (HIn (Digit (HIn (Node2     c b)))))) |> a = deep pr m                  (digit (node3   c b a))
(HIn (Deep pr m (HIn (Digit (HIn (Node3   d c b)))))) |> a = deep pr m                  (       node4 d c b a)
(HIn (Deep pr m (HIn             (Node4 e d c b  )))) |> a = deep pr (node3 e d c <| m) (digit (node2 b a    ))
(HIn (Single b                                     )) |> a = deep b  empty_             (node1 a)
(HIn  Empty                                         ) |> a = single (node1 a)
x |> _ = x

instance HFunctor (Finger a) where
  hfmap _ Empty           = Empty
  hfmap f (Single a)      = Single (f a)
  hfmap f (Deep a c b)    = Deep (f a) (f c) (f b)
  hfmap f (Digit a)       = Digit (f a)
  hfmap _ (Value a)       = Value a
  hfmap f (Node1 a)       = Node1 (f a)
  hfmap f (Node2 a b)     = Node2 (f a) (f b)
  hfmap f (Node3 a b c)   = Node3 (f a) (f b) (f c)
  hfmap f (Node4 a b c d) = Node4 (f a) (f b) (f c) (f d)

instance HFoldable (Finger a) where
  hfoldMap _ Empty           = mempty
  hfoldMap f (Single a)      = f a
  hfoldMap f (Deep a c b)    = mconcat [f a, f c, f b]
  hfoldMap f (Digit a)       = f a
  hfoldMap _ (Value _)       = mempty
  hfoldMap f (Node1 a)       = f a
  hfoldMap f (Node2 a b)     = mconcat [f a, f b]
  hfoldMap f (Node3 a b c)   = mconcat [f a, f b, f c]
  hfoldMap f (Node4 a b c d) = mconcat [f a, f b, f c, f d]

instance HTraversable (Finger a) where
  htraverse _ Empty           = pure Empty
  htraverse f (Single a)      = Single <$> f a
  htraverse f (Deep a c b)    = Deep <$> f a <*> f c <*> f b
  htraverse f (Digit a)       = Digit <$> f a
  htraverse _ (Value a)       = pure (Value a)
  htraverse f (Node1 a)       = Node1 <$> f a
  htraverse f (Node2 a b)     = Node2 <$> f a <*> f b
  htraverse f (Node3 a b c)   = Node3 <$> f a <*> f b <*> f c
  htraverse f (Node4 a b c d) = Node4 <$> f a <*> f b <*> f c <*> f d

-------------------

myFinger :: Root Int
myFinger = deep a (deep c empty_ d) b
  where
  a = digit (value 1)
  b = digit (value 2)
  c = digit (node3 (value 1) (value 4) (value 6))
  d = node1 (value 1)

