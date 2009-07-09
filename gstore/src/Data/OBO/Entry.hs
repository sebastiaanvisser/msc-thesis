{-# LANGUAGE
    TypeFamilies
  , TemplateHaskell
  , GADTs
  , KindSignatures
  , EmptyDataDecls
  , TypeSynonymInstances
  , MultiParamTypeClasses
  #-}
module Data.OBO.Entry where

import Data.Binary
import Data.OBO.Document
import Generics.Regular.Base
import Generics.Regular.Binary
import Generics.Regular.TH

-- Single ontology entry.

data Entry = 
  En {
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

$(deriveConstructors ''Entry)
$(deriveRegular ''Entry "PFEntry")
type instance PF Entry = PFEntry

instance Eq Entry where
  a == b = name a == name b

instance Ord Entry where
  a `compare` b = name a `compare` name b

-- Generic view on entries.

instance Binary Entry where
  put = gput
  get = gget

-- Convert a generic OBO stanza to a specific entry.

stanzaToEntry :: Stanza -> Entry
stanzaToEntry st =
  let m t = (maybe "" id . lookup t . stMap) st
  in En {
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

testEntry :: Entry
testEntry =
  En {
    alt_id        = "alt_id"
  , comment       = "comment"
  , consider      = "consider"
  , def           = "def"
  , _id           = "_id"
  , is_a          = "is_a"
  , is_obsolete   = "is_obsolete"
  , name          = "name"
  , namespace     = "namespace"
  , relationship  = "relationship"
  , replaced_by   = "replaced_by"
  , subset        = "subset"
  , synonym       = "synonym"
  , xref          = "xref"
  }

