{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , EmptyDataDecls
  , GADTs
  , KindSignatures
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , TypeOperators
 #-}
module Container.FingerTree.Abstract where

import Prelude hiding (foldr, foldl, sum, lookup)
import Control.Applicative
import Data.Foldable hiding (toList, sum)
import Data.Monoid
import Generics.Types

-- Nodes in a finger tree can represent a part of the spine or a part of the
-- fingers. The top level of the fingers are the digits which can have a branch
-- level of 1-4. The lower levels of the fingers are nodes which can have a 2-3
-- branch level. At the lowest level are the values.

data Sp
data Dg
data Nd

data Tree (a :: *) (f :: * -> *) :: * -> * where
  Empty  ::                                                     Tree a f (Sp, c)
  Single :: f (Dg, c)                                        -> Tree a f (Sp, c)
  Deep   :: f (Dg, c) -> f (Sp, Succ c) -> f (Dg, c)         -> Tree a f (Sp, c)

  Digit1 :: f (Nd, c)                                        -> Tree a f (Dg, Succ c)
  Digit2 :: f (Nd, c) -> f (Nd, c)                           -> Tree a f (Dg, Succ c)
  Digit3 :: f (Nd, c) -> f (Nd, c) -> f (Nd, c)              -> Tree a f (Dg, Succ c)
  Digit4 :: f (Nd, c) -> f (Nd, c) -> f (Nd, c) -> f (Nd, c) -> Tree a f (Dg, Succ c)

  Node2  :: f (Nd, c) -> f (Nd, c)                           -> Tree a f (Nd, Succ c)
  Node3  :: f (Nd, c) -> f (Nd, c) -> f (Nd, c)              -> Tree a f (Nd, Succ c)
  Value  :: a                                                -> Tree a f (Nd, Zero)

-- Pretty names for common structures.

type Node       a c = HFix (Tree a) (Nd, c)
type Value      a   = Node a Zero
type Digit      a c = HFix (Tree a) (Dg, c)
type Spine      a c = HFix (Tree a) (Sp, c)
type FingerTree a   = HFix (Tree a) (Sp, One)

-- Bunch of smart constructors taking into the account the fixed point constructor HIn.

empty_ :: Spine a c
empty_ = HIn Empty

single :: Digit a c -> Spine a c
single a = HIn (Single a)

deep :: Digit a c -> Spine a (Succ c) -> Digit a c -> Spine a c
deep a c b = HIn (Deep a c b)

digit1 :: Node a c -> Digit a (Succ c)
digit1 a = HIn (Digit1 a)

digit2 :: Node a c -> Node a c -> Digit a (Succ c)
digit2 a b = HIn (Digit2 a b)

digit3 :: Node a c -> Node a c -> Node a c -> Digit a (Succ c)
digit3 a b c = HIn (Digit3 a b c)

digit4 :: Node a c -> Node a c -> Node a c -> Node a c -> Digit a (Succ c)
digit4 a b c d = HIn (Digit4 a b c d)

node2 :: Node a c -> Node a c -> Node a (Succ c)
node2 a b = HIn (Node2 a b)

node3 :: Node a c -> Node a c -> Node a c -> Node a (Succ c)
node3 a b c = HIn (Node3 a b c)

value :: a -> Value a
value i = HIn (Value i)

-- Higher order functor, foldable and traversable instances.

instance HFunctor (Tree a) where
  hfmap _ (Empty         ) = Empty
  hfmap f (Single a      ) = Single (f a)
  hfmap f (Deep   a c b  ) = Deep   (f a) (f c) (f b)
  hfmap f (Digit1 a      ) = Digit1 (f a)
  hfmap f (Digit2 a b    ) = Digit2 (f a) (f b)
  hfmap f (Digit3 a b c  ) = Digit3 (f a) (f b) (f c)
  hfmap f (Digit4 a b c d) = Digit4 (f a) (f b) (f c) (f d)
  hfmap f (Node2  a b    ) = Node2  (f a) (f b)
  hfmap f (Node3  a b c  ) = Node3  (f a) (f b) (f c)
  hfmap _ (Value  a      ) = Value  a

instance HFoldable (Tree a) where
  hfoldMap _ (Empty         ) = mempty
  hfoldMap f (Single a      ) =          f a
  hfoldMap f (Deep   a c b  ) = mconcat [f a, f c, f b]
  hfoldMap f (Digit1 a      ) =          f a
  hfoldMap f (Digit2 a b    ) = mconcat [f a, f b]
  hfoldMap f (Digit3 a b c  ) = mconcat [f a, f b, f c]
  hfoldMap f (Digit4 a b c d) = mconcat [f a, f b, f c, f d]
  hfoldMap f (Node2  a b    ) = mconcat [f a, f b]
  hfoldMap f (Node3  a b c  ) = mconcat [f a, f b, f c]
  hfoldMap _ (Value  _      ) = mempty

instance HTraversable (Tree a) where
  htraverse _ (Empty         ) = C (pure Empty)
  htraverse f (Single a      ) = C (Single <$> unC (f a))
  htraverse f (Deep   a c b  ) = C (Deep   <$> unC (f a) <*> unC (f c) <*> unC (f b))
  htraverse f (Digit1 a      ) = C (Digit1 <$> unC (f a))
  htraverse f (Digit2 a b    ) = C (Digit2 <$> unC (f a) <*> unC (f b))
  htraverse f (Digit3 a b c  ) = C (Digit3 <$> unC (f a) <*> unC (f b) <*> unC (f c))
  htraverse f (Digit4 a b c d) = C (Digit4 <$> unC (f a) <*> unC (f b) <*> unC (f c) <*> unC (f d))
  htraverse f (Node2  a b    ) = C (Node2  <$> unC (f a) <*> unC (f b))
  htraverse f (Node3  a b c  ) = C (Node3  <$> unC (f a) <*> unC (f b) <*> unC (f c))
  htraverse _ (Value  a      ) = C (pure (Value a))

-- Left and right biased insertions.

infixr 5 <|

(<|) :: Node a c -> Spine a (Succ c) -> Spine a (Succ c)
a <| (HIn (Deep ( HIn (Digit1 b      ))   m sf)) = deep   (digit2 a b    ) m                  sf
a <| (HIn (Deep ( HIn (Digit2 b c    ))   m sf)) = deep   (digit3 a b c  ) m                  sf
a <| (HIn (Deep ( HIn (Digit3 b c d  ))   m sf)) = deep   (digit4 a b c d) m                  sf
a <| (HIn (Deep ( HIn (Digit4 b c d e))   m sf)) = deep   (digit2 a b    ) (node3 c d e <| m) sf
a <| (HIn (Single b                           )) = deep   (digit1 a)       empty_             b
a <| (HIn (Empty                              )) = single (digit1 a)

infixr 5 |>

(|>) :: Spine a (Succ c) -> Node a c -> Spine a (Succ c)
(HIn (Deep pr m (HIn (Digit1       b  )))) |> a = deep   pr m                  (digit2     b a)
(HIn (Deep pr m (HIn (Digit2     c b  )))) |> a = deep   pr m                  (digit3   c b a)
(HIn (Deep pr m (HIn (Digit3   d c b  )))) |> a = deep   pr m                  (digit4 d c b a)
(HIn (Deep pr m (HIn (Digit4 e d c b  )))) |> a = deep   pr (node3 e d c <| m) (digit2     b a)
(HIn (Single b                          )) |> a = deep   b  empty_             (digit1       a)
(HIn (Empty                             )) |> a = single                       (digit1       a)

(|<|) :: Foldable f => f (Value a) -> FingerTree a -> FingerTree a
(|<|) = flip (foldr (<|))

(|>|) :: Foldable f => f (Value a) -> FingerTree a -> FingerTree a
(|>|) = flip (foldl (|>))

fromList :: [Value a] -> FingerTree a
fromList = (|<| empty_)

getValue :: Tree a f ix -> [a]
getValue (Value a) = pure a
getValue _         = mempty

toList :: HFix (Tree a) c -> [a]
toList = foldm getValue

-------------------

test :: FingerTree Int
test = fromList (map value [3, 12, 44, 5, 2, 100, 20])

sumAlg :: HAlg (Tree Int) (K Int)
sumAlg (Empty         ) = K 0
sumAlg (Single a      ) = K (unK a)
sumAlg (Deep   a b c  ) = K (unK a + unK b + unK c)
sumAlg (Digit1 a      ) = K (unK a)
sumAlg (Digit2 a b    ) = K (unK a + unK b)
sumAlg (Digit3 a b c  ) = K (unK a + unK b + unK c)
sumAlg (Digit4 a b c d) = K (unK a + unK b + unK c + unK d)
sumAlg (Node2  a b    ) = K (unK a + unK b)
sumAlg (Node3  a b c  ) = K (unK a + unK b + unK c)
sumAlg (Value  a      ) = K a

sum :: FingerTree Int -> Int
sum = unK . hfold (sumAlg)

containsAlg :: Eq a => a -> HAlg (Tree a) (K Bool)
containsAlg _ (Empty         ) = K False
containsAlg _ (Single a      ) = K (unK a)
containsAlg _ (Deep   a b c  ) = K (unK a || unK b || unK c)
containsAlg _ (Digit1 a      ) = K (unK a)
containsAlg _ (Digit2 a b    ) = K (unK a || unK b)
containsAlg _ (Digit3 a b c  ) = K (unK a || unK b || unK c)
containsAlg _ (Digit4 a b c d) = K (unK a || unK b || unK c || unK d)
containsAlg _ (Node2  a b    ) = K (unK a || unK b)
containsAlg _ (Node3  a b c  ) = K (unK a || unK b || unK c)
containsAlg v (Value  a      ) = K (a == v)

contains :: Eq a => a -> FingerTree a -> Bool
contains v = unK . hfold (containsAlg v)

lookupAlg :: Eq a => a -> HAlg (Tree (a, b)) (K (Maybe b))
lookupAlg _ (Empty         ) = K Nothing
lookupAlg _ (Single a      ) = castK a
lookupAlg _ (Deep   a b c  ) = K (unK a <|> unK b <|> unK c)
lookupAlg _ (Digit1 a      ) = K (unK a)
lookupAlg _ (Digit2 a b    ) = K (unK a <|> unK b)
lookupAlg _ (Digit3 a b c  ) = K (unK a <|> unK b <|> unK c)
lookupAlg _ (Digit4 a b c d) = K (unK a <|> unK b <|> unK c <|> unK d)
lookupAlg _ (Node2  a b    ) = K (unK a <|> unK b)
lookupAlg _ (Node3  a b c  ) = K (unK a <|> unK b <|> unK c)
lookupAlg v (Value  (a, b) ) = K (if a == v then Just b else Nothing)

lookup :: Eq a => a -> FingerTree (a, b) -> Maybe b
lookup v = unK . hfold (lookupAlg v)

data N   f ix = N   { unN   :: Maybe (f (Nd, Snd ix))    }
data Inc f ix = Inc { unInc :: f (Fst ix, Succ (Snd ix)) }
data Dec f ix = Dec { unDec :: f (Fst ix, Pred (Snd ix)) }

insertAlg
  :: f ~ Tree a
  => t ~ HFix f
  => r ~ (N (Dec t) :-> t :*: N t)
  => Inc (f (t :*: r)) :~> Inc r

insertAlg (Inc (Empty         )) = Inc . F $ maybe (empty_                                     :*: N Nothing) (\(Dec z) -> single (digit1 z)                   :*: N Nothing                                   ) . unN
insertAlg (Inc (Single b      )) = Inc . F $ maybe (single (hfst b)                            :*: N Nothing) (\(Dec z) -> deep (hfst b) empty_ (digit1 z)     :*: N Nothing                                   ) . unN
insertAlg (Inc (Digit1 b      )) = Inc . F $ maybe (digit1 (hfst b)                            :*: N Nothing) (\(Dec z) -> digit2 z (hfst b)                   :*: N Nothing                                   ) . unN
insertAlg (Inc (Digit2 b c    )) = Inc . F $ maybe (digit2 (hfst b) (hfst c)                   :*: N Nothing) (\(Dec z) -> digit3 z (hfst b) (hfst c)          :*: N Nothing                                   ) . unN
insertAlg (Inc (Digit3 b c d  )) = Inc . F $ maybe (digit3 (hfst b) (hfst c) (hfst d)          :*: N Nothing) (\(Dec z) -> digit4 z (hfst b) (hfst c) (hfst d) :*: N Nothing                                   ) . unN
insertAlg (Inc (Digit4 b c d e)) = Inc . F $ maybe (digit4 (hfst b) (hfst c) (hfst d) (hfst e) :*: N Nothing) (\(Dec z) -> digit2 z (hfst b)                   :*: N (Just (node3 (hfst c) (hfst d) (hfst e))) ) . unN
insertAlg (Inc (Node2  b c    )) = Inc . F $ maybe (node2  (hfst b) (hfst c)                   :*: N Nothing) (\(Dec z) -> node3  z (hfst b) (hfst c)          :*: N Nothing                                   ) . unN
insertAlg (Inc (Node3  b c d  )) = Inc . F $ maybe (node3  (hfst b) (hfst c) (hfst d)          :*: N Nothing) (\(Dec z) -> node2  z (hfst b)                   :*: N (Just (node2 (hfst c) (hfst d)))          ) . unN
insertAlg (Inc (Deep   b m sf )) = Inc . F $ maybe (deep   (hfst b) (hfst m) (hfst sf)         :*: N Nothing) (\z -> let _b = hsnd b # N (Just z)
                                                                                                                         _m = hsnd m # N (fmap Dec (unN (hsnd _b)))
                                                                                                                     in deep (hfst b) (hfst _m) (hfst sf)      :*: N Nothing                                   ) . unN

