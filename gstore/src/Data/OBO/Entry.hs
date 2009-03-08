module Data.OBO.Entry where

import Data.OBO.Document
import Data.OBO.Parser

data Entry =
  Entry {
    alt_id       :: String
  , comment      :: String
  , consider     :: String
  , def          :: String
  , _id          :: String
  , is_a         :: String
  , is_obsolete  :: String
  , name         :: String
  , namespace    :: String
  , relationship :: String
  , replaced_by  :: String
  , subset       :: String
  , synonym      :: String
  , xref         :: String
  } deriving (Eq, Ord, Show)

stanzaToEntry :: Stanza -> Entry
stanzaToEntry st =
  let m t = (maybe "" id . lookup t . stMap) st
  in Entry {
       alt_id       = m "alt_id"
     , comment      = m "comment"
     , consider     = m "consider"
     , def          = m "def"
     , _id          = m "id"
     , is_a         = m "is_a"
     , is_obsolete  = m "is_obsolete"
     , name         = m "name"
     , namespace    = m "namespace"
     , relationship = m "relationship"
     , replaced_by  = m "replaced_by"
     , subset       = m "subset"
     , synonym      = m "synonym"
     , xref         = m "xref"
     } 

main :: IO ()
main = do
  file <- readFile "../data/cellular_component.obo"
  let k = parseOBO file
  case k of
    Left e    -> print e
    Right doc -> 
      do let stanzas = docStanzas doc
             entries = map stanzaToEntry stanzas
         mapM_ (putStrLn . _id) entries

