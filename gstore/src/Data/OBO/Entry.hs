{-# LANGUAGE TypeFamilies #-}
module Data.OBO.Entry where

import Data.Binary
import Data.Binary.Generic
import Data.OBO.Document
import Generic.Representation

-- Single ontology entry.

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
  } deriving Show

instance Eq Entry where
  a == b = name a == name b

instance Ord Entry where
  a `compare` b = name a `compare` name b

-- Generic view on entries.

instance PFView Entry where
  type PF Entry = {- the pain! -}
    (Prod (Prod (Prod (Prod (K String) (K String)) (Prod (K String) (K String)))
                (Prod (Prod (K String) (K String)) (Prod (K String) (K String))))
          (Prod (Prod (Prod (K String) (K String)) (Prod (K String) (K String)))
                (Prod (K String) (K String))))

  from (Entry e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13) =
     (Prod (Prod (Prod (Prod (K e0) (K e1)) (Prod (K e2) (K e3)))
                 (Prod (Prod (K e4) (K e5)) (Prod (K e6) (K e7))))
           (Prod (Prod (Prod (K e8) (K e9)) (Prod (K e10) (K e11)))
                 (Prod (K e12) (K e13))))

  to (Prod (Prod (Prod (Prod (K e0) (K e1)) (Prod (K e2) (K e3)))
                 (Prod (Prod (K e4) (K e5)) (Prod (K e6) (K e7))))
           (Prod (Prod (Prod (K e8) (K e9)) (Prod (K e10) (K e11)))
                 (Prod (K e12) (K e13)))) =
        Entry e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13

-- Generically derived Binary instance.

instance Binary Entry where
  put = gput
  get = gget

-- Convert a generic OBO stanza to a specific entry.

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

