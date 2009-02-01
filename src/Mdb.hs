module Mdb where

import Prelude hiding (lookup)
import Storage
import Tree

type Title    = String
type Director = String
type Actor    = String

data Movie =
  Movie {
    title    :: Title
  , director :: Director
  , cast     :: [Actor]
  } deriving (Show, Read)

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

insertWithTitle :: Movie -> Database -> Database
insertWithTitle = insertWith title

mdb :: Database
mdb = 
    insertWithTitle jurassicPark
  $ insertWithTitle anchorMan
  $ insertWithTitle zoolander
  $ empty

directorOf :: String -> String
directorOf movie = ("not found" `maybe` director) (lookup movie mdb)

-------------------------------------------------------------------------------

myCharList = "The quick brown fox jumped over the lazy dog!"

numTree = fromList $ zip myCharList [0..]


