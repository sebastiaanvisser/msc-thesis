module GenericXML where

import Data
import Generic
import XML

class XML a where
  xml :: a -> Node

instance XML Int where
  xml i = ElementNode (element "Int" << i)

instance XML Bool where
  xml b = ElementNode (element "Bool" << b)

instance XML String where
  xml s = ElementNode (element "String" << s)

-- Generic XML printing.

class GXML f where
  gxml :: (a -> NodeSet) -> f a -> NodeSet

instance GXML Id where
  gxml f (Id r) = f r

instance XML a => GXML (K a) where
  gxml _ (K x) = NodeSet [xml x]

instance GXML Unit where
  gxml _ Unit = NodeSet []

instance (GXML f, GXML g) => GXML (Sum f g) where
  gxml f (Inl x) = gxml f x
  gxml f (Inr x) = gxml f x

instance (GXML f, GXML g) => GXML (Prod f g) where
  gxml f (Prod x y) = gxml f x `catNodeSets` gxml f y

instance GXML f => GXML (Con f) where
  gxml f (Con c x) = NodeSet [ElementNode $ element c << gxml f x]

toXML :: (GXML (PF a), PFView a) => a -> NodeSet
toXML = gxml toXML . from

