{-# LANGUAGE
    TemplateHaskell
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
 #-}
module Container.Tree.Abstract where

import Data.Binary
import Data.Foldable hiding (all)
import Data.Traversable
import Generics.Regular.Binary
import Generics.Regular.TH
import Generics.Types
import qualified Generics.Regular as R

-- | Binary tree parametrized by key type, value type and recursive positions.

{-data Balance = BLL | BL | B | BR | BRR
  deriving (Eq, Ord, Show, Read)

$(deriveAll ''Balance "PFBalance")
type instance R.PF Balance = PFBalance

instance Binary Balance where
  put = gput
  get = gget

rLeft :: Balance -> Balance
rLeft BLL = BLL
rLeft BL  = BLL
rLeft B   = BL
rLeft BR  = B
rLeft BRR = BR

rRight :: Balance -> Balance
rRight BLL = BL
rRight BL  = B
rRight B   = BR
rRight BR  = BRR
rRight BRR = BRR
-}

data Tree k v f =
    Leaf
  | Branch
    { key     :: k
    , val     :: v
    , leftT   :: f
    , rightT  :: f
    }
  deriving (Eq, Ord, Read, Functor, Foldable, Traversable)

instance (Show k, Show v, Show f) => Show (Tree k v f) where
  show Leaf = ""
  show (Branch k v l r) =
            indent (show r) ++
    "\n" ++ show k ++ " = " ++ show v
         ++ indent (show l)

indent :: String -> String
indent = unlines . map ("\t"++) . lines 

$(deriveAll ''Tree "PFTree")
type instance R.PF (Tree k v f) = PFTree k v f

instance (Binary k, Binary v, Binary f) => Binary (Tree k v f) where
  put = gput
  get = gget

instance (Show k, Show v) => Show (FixA Id (Tree k v)) where
  show = show . unId . out

