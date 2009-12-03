module TestAlg where

import Control.Applicative
import Control.Monad.Identity
import Generics.Types
import qualified Generics.Morphism.Para as Para
import qualified Container.Tree.Abstract as F

type Tree    = FixA Id (F.Tree () Int)
type Alg r   = Para.Alg (F.Tree () Int) r
type EndoAlg = Para.Endo (F.Tree () Int)

-- constructors

leaf :: Tree
leaf = (In . Id) F.Leaf

branch :: Int -> Tree -> Tree -> Tree
branch i a b = (In . Id) (F.Branch () i a b)

single :: Int -> Tree
single a = branch a leaf leaf

tri :: Int -> Int -> Int -> Tree
tri a b c = branch b (single a) (single c)

-- repmin

-- minAlg :: Alg Int
-- minAlg = Para.Psi $ \a ->
--   case fst a of
--     F.Leaf           -> maxBound
--     F.Branch _ v l r -> minimum [v, l, r]

-- repAlg :: Alg (Int -> Tree)
-- repAlg = Para.Psi $ \a x ->
--   case fst a of
--     F.Leaf           -> In (Id (F.Leaf))
--     F.Branch k _ l r -> In (Id (F.Branch k x (l x) (r x)))

-- repMinAlg :: Alg Tree
-- repMinAlg = repAlg <*> minAlg

-- two repmins.

-- runRepMinAsPara :: Tree -> Tree
-- runRepMinAsPara = runIdentity . Para.paraMA repMinAlg

-- runRepMinAsEndo :: Tree -> Tree
-- runRepMinAsEndo = runIdentity . Para.endoMA (Para.toEndo repMinAlg)




-- test tree.


myT :: Tree
myT =
  branch 40 (branch 6  (tri 1 2 3)
                       (tri 8 9 10))
            (branch 86 (tri 81 82 83)
                       (tri 88 89 90))

