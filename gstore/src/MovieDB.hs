{-# LANGUAGE TypeFamilies #-}
module MovieDB where

import Container.Tree
import Data.Binary
import Data.Binary.Generic
import Generic.Core
import Prelude hiding (lookup)

type Title    = String
type Director = String
type Actor    = String

data Movie =
    Ref Title
  | Movie {
      title    :: Title
    , director :: Director
    , cast     :: [Actor]
    } deriving (Show, Read)

instance PFView Movie where
  type PF Movie =  Sum (K Title) (Prod (K Title) (Prod (K Director) (K [Actor])))

  from (Ref t)       = Inl (K t)
  from (Movie t d c) = Inr (Prod (K t) (Prod (K d) (K c)))

  to (Inl (K t))                           = Ref t
  to (Inr (Prod (K t) (Prod (K d) (K c)))) = Movie t d c

instance Binary Movie where
  put = gput
  get = gget

instance Eq Movie where
  a == b = title a == title b

instance Ord Movie where
  a `compare` b = title a `compare` title b

type Database = Tree Title Movie

emptyMovie :: Movie
emptyMovie = Movie "" "" []

