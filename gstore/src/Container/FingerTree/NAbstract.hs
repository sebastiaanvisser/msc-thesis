{-# LANGUAGE
    TemplateHaskell
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Container.FingerTree.NAbstract where

-- import Control.Applicative
-- import Data.Monoid
import Data.Foldable
import Data.Traversable
import Generics.Types

data Node f = Node f f
  deriving Functor

data Finger n f = Empty | Single n | Deep n f n
  deriving (Functor, Foldable, Traversable)

type instance Nest (Finger n) = Finger (Node n)

type NFingerA a n = NFixA a (Finger n)




{-inid :: h (HFixA HId h) ix -> HFixA HId h ix
inid = HIn . HId

htest :: HFixA HId Finger Int
htest = inid (Deep 2 (inid (Deep (Node 3 4) (inid (Deep xx (inid Empty) yy)) (Node 4 5))) 4)
  where xx = Node (Node 10 11) (Node 12 13)
        yy = Node (Node 14 15) (Node 16 17)


folder :: (Num t) => [t]
folder = hfoldMap (\x -> let _x = x :: ix a in [20]) (unHId (hout htest))

tak :: Finger t a -> [a]
tak Empty        = []
tak (Single a)   = [a]
tak (Deep a _ b) = [a, b]
-}


