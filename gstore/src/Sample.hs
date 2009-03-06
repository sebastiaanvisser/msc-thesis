{-# LANGUAGE TypeFamilies #-}

module Sample where

import Container.Tree
import Storage.Storage
import MovieDB
import Prelude hiding (lookup)

tri :: Storage t (Persistent (Tree Title Movie))
tri = triplet
  "anch" anchorMan
  "jura" jurassicPark
  "zool" zoolander

jurassicPark, anchorMan, zoolander :: Movie
jurassicPark = Movie "Jurassic Park" "Steven Spielberg" ["Sam Neill", "Richard Attenborough"]
anchorMan    = Movie "Anchorman"     "Adam McKay"       ["Will Ferell", "Christina Applegate", "Steve Carell"]
zoolander    = Movie "Zoolander"     "Geen idee"        ["Ben Stiller"]

-- myCharList :: String
-- myCharList = "The quick brown fox jumped over the lazy dog!"

-- numDB :: Tree Char Integer
-- numDB = fromList $ zip myCharList [0..]

-- mytest0 :: (Maybe Integer, String)
-- mytest0 = traceLookup 'q' numDB :: (Maybe Integer, [Char])

-- mytest1 :: IO (Maybe Integer)
-- mytest1 = ioFixLookup 'q' numDB :: IO (Maybe Integer)

