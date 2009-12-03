{-# LANGUAGE
    TemplateHaskell
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Container.FingerTree.Abstract where

import Generics.Types
import Data.Foldable
import Data.Traversable

data Finger f a = Empty | Single a | Deep a (f (Node a)) a
  deriving (Functor, Foldable, Traversable)

data Node f = Node f f
  deriving (Functor, Foldable, Traversable)

type HFinger a = HFix Finger a

inid :: h (HFixA HId h) ix -> HFixA HId h ix
inid = HIn . HId

htest :: HFixA HId Finger Int
htest = inid (Deep 2 (inid (Deep (Node 3 4) (inid (Deep xx (inid Empty) yy)) (Node 4 5))) 4)
  where xx = Node (Node 10 11) (Node 12 13)
        yy = Node (Node 14 15) (Node 16 17)

