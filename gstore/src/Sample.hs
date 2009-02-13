{-# LANGUAGE TypeFamilies #-}

module Sample where

import Data.Binary
import Container.Tree
-- import Generic.Arbitrary
import MovieDB
import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy as B

jurassicPark, anchorMan, zoolander :: Movie
jurassicPark = Movie "Jurassic Park" "Steven Spielberg" ["Sam Neill", "Richard Attenborough"]
anchorMan    = Movie "Anchorman"     "Adam McKay"       ["Will Ferell", "Christina Applegate", "Steve Carell"]
zoolander    = Movie "Zoolander"     "Geen idee"        ["Ben Stiller"]

mDB :: Tree Title Movie
mDB = 
    insertWith title anchorMan
  $ insertWith title jurassicPark
  $ insertWith title zoolander
  $ empty

directorOf :: Title -> [Char]
directorOf movie = ("not found" `maybe` director) (lookup movie mDB)

myCharList :: String
myCharList = "The quick brown fox jumped over the lazy dog!"

numDB :: Tree Char Integer
numDB = fromList $ zip myCharList [0..]

mytest0 :: (Maybe Integer, String)
mytest0 = traceLookup 'q' numDB :: (Maybe Integer, [Char])

mytest1 :: IO (Maybe Integer)
mytest1 = ioFixLookup 'q' numDB :: IO (Maybe Integer)

t :: FTree String Movie ()
t = Branch "Jura" jurassicPark () ()

encodeDB :: B.ByteString
encodeDB = encode t

