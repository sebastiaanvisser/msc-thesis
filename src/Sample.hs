module Sample where

import Prelude hiding (lookup)
import Container.Tree
import Generic.Annotate

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

mDB :: Database
mDB = 
    insertWith title anchorMan
  $ insertWith title jurassicPark
  $ insertWith title zoolander
  $ empty

directorOf :: String -> String
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


