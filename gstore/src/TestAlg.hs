module TestAlg where

import Control.Applicative
import Control.Monad.Identity
import Generics.Types
import Generics.Morph
import qualified Container.Tree.Abstract as F

type Tree    = FixT  Id (F.Tree () Int)
type Tree1   = FixT1 Id (F.Tree () Int)
type Alg r   = Psi   Id (F.Tree () Int) r
type EndoAlg = Endo Id (F.Tree () Int)

leaf :: Tree
leaf = (In . Id) F.Leaf

branch :: Int -> Tree -> Tree -> Tree
branch i a b = (In . Id) (F.Branch F.B () i a b)

single :: Int -> Tree
single a = branch a leaf leaf

tri :: Int -> Int -> Int -> Tree
tri a b c = branch b (single a) (single c)

minAlg :: Alg Int
minAlg = Psi $ \a ->
  case fst a of
    F.Leaf             -> maxBound
    F.Branch _ _ v l r -> minimum [v, l, r]

repAlg :: Alg (Int -> Tree)
repAlg = Psi $ \a x ->
  case fst a of
    F.Leaf             -> In (Id (F.Leaf))
    F.Branch b k _ l r -> In (Id (F.Branch b k x (l x) (r x)))

repMinAlg :: Alg Tree
repMinAlg = repAlg <*> minAlg


runRepMinAsPara :: Tree -> Tree
runRepMinAsPara = runIdentity . paraMT repMinAlg . out

runRepMinAsEndo :: Tree -> Tree
runRepMinAsEndo = In . runIdentity . endoMT (toEndo repMinAlg) . out


myT :: Tree
myT =
  branch 40
    (branch 6
      (tri 1 2 3)
      (tri 8 9 10))
    (branch 86
      (tri 81 82 83)
      (tri 88 89 90))
