{-# LANGUAGE TypeFamilies #-}

module Sample where

import Container.Tree
import Generic.Annotate
import Generic.Core
import Generic.Persist
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

-- Compare movies by title.
instance Eq Movie where
  a == b = title a == title b

instance Ord Movie where
  a `compare` b = title a `compare` title b

type Database = Tree Title Movie

emptyMovie :: Movie
emptyMovie = Movie "" "" []

jurassicPark, anchorMan, zoolander :: Movie
jurassicPark = Movie "Jurassic Park" "Steven Spielberg" ["Sam Neill", "Richard Attenborough"]
anchorMan    = Movie "Anchorman"     "Adam McKay"       ["Will Ferell", "Christina Applegate", "Steve Carell"]
zoolander    = Movie "Zoolander"     "Geen idee"        ["Ben Stiller"]

mDB = 
    insertWith title anchorMan
  $ insertWith title jurassicPark
  $ insertWith title zoolander
  $ empty

directorOf movie = ("not found" `maybe` director) (lookup movie mDB)

-------------------------------------------------------------------------------

myCharList :: String
myCharList = "The quick brown fox jumped over the lazy dog!"

numDB :: Tree Char Integer
numDB = fromList $ zip myCharList [0..]

mytest0 :: (Maybe Integer, String)
mytest0 = traceLookup 'q' numDB :: (Maybe Integer, [Char])

mytest1 :: IO (Maybe Integer)
mytest1 = ioFixLookup 'q' numDB :: IO (Maybe Integer)

