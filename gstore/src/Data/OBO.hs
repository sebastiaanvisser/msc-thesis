module Data.OBO (

    Header
  , Name
  , Tag
  , Value
  , TagValues
  , Document (..)
  , Stanza (..)

  , parseOBO

  , Entry (..)
  , stanzaToEntry

  ) where

import Data.OBO.Document
import Data.OBO.Parser
import Data.OBO.Entry

