module Data.OBO.Document where

-- OBO data type definitions.

type Header    = String
type Name      = String
type Tag       = String
type Value     = String
type TagValues = [(Tag, Value)]

data Document =
  Document {
    docHeader  :: TagValues
  , docStanzas :: [Stanza]
  } deriving (Eq, Ord, Show)

data Stanza =
  Stanza {
    stName :: Name
  , stMap  :: TagValues
  } deriving (Eq, Ord, Show)

