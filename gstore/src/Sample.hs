{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Sample where

import Container.Tree
import Storage.Storage
import MovieDB
import Prelude hiding (lookup)

instance TreeClass [Char]
instance TreeClass Movie

-- tri :: Storage t (Pointer (Tree Title Movie))
tri = triplet
  "anch" anchorman
  "jura" jurassicpark
  "zool" zoolander

jurassicpark, anchorman, zoolander :: Movie
jurassicpark = Movie "Jurassic Park" "Steven Spielberg" ["Sam Neill", "Richard Attenborough"]
anchorman    = Movie "Anchorman"     "Adam McKay"       ["Will Ferell", "Christina Applegate", "Steve Carell"]
zoolander    = Movie "Zoolander"     "Geen idee"        ["Ben Stiller"]

-- myCharList :: String
-- myCharList = "The quick brown fox jumped over the lazy dog!"

-- numDB :: Tree Char Integer
-- numDB = fromList $ zip myCharList [0..]

-- mytest0 :: (Maybe Integer, String)
-- mytest0 = traceLookup 'q' numDB :: (Maybe Integer, [Char])

-- mytest1 :: IO (Maybe Integer)
-- mytest1 = ioFixLookup 'q' numDB :: IO (Maybe Integer)

