{-# LANGUAGE TemplateHaskell #-}
module Heap.Block where

import Data.Binary
import Data.ByteString.Lazy
import Data.Record.Label

type Offset = Int
type Size   = Int
type Header = (Bool, Size)

-- todo: binary instance!!
data Block =
  Block
  { _offset  :: Offset
  , _size    :: Size
  , _payload :: Maybe ByteString
  } deriving (Eq, Ord, Show)

$(mkLabels [''Block])

payload  :: Block :-> Maybe ByteString
size     :: Block :-> Size
offset   :: Block :-> Offset

newtype Pointer (f :: * -> *) a = Ptr { unPtr :: Offset }
  deriving (Show, Binary)

nullPtr :: Pointer f a
nullPtr = Ptr 0

-- Constant values.

splitThreshold :: Size
splitThreshold = 4

headerSize :: Size
headerSize = 1 -- Occupied flag.
           + 4 -- Block size.

