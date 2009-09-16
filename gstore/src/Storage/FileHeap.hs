{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}

module Storage.FileHeap (
    Heap
  , runHeap
  , location

  , unsafeReadSize
  , writePayload

  , allocate
  , free
  , write
  , read

  , dump
  , dumpAllocationMap
  ) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.State as T
import Data.Maybe
import Data.Record.Label
import Data.Word
import Prelude hiding (read)
import Storage.FileIO
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as I

-- Data types.

type Offset  = Int
type Size    = Int
type Header  = (Bool, Size)

data Block =
  Block {
    _offset   :: Offset
  , _size     :: Size
  , _payload  :: Maybe B.ByteString
  } deriving (Eq, Ord, Show)

data FileHeap = 
  FileHeap {
    _allocMap :: I.IntMap [Int]
  , _heapSize :: Int
  , _file     :: Handle
  } deriving Show

type Heap m = T.StateT FileHeap IO m

$(mkLabels [''Block, ''FileHeap])

file     :: FileHeap :-> Handle
heapSize :: FileHeap :-> Size
allocMap :: FileHeap :-> I.IntMap [Int]
payload  :: Block :-> Maybe B.ByteString
size     :: Block :-> Size
offset   :: Block :-> Offset

-- Helper functions.

location :: Block -> Offset
location = get offset

headerSize :: Size
headerSize = 1 + 4

splitThreshold :: Size
splitThreshold = 4

blockSize :: Block -> Size
blockSize = (+headerSize) . get size

nextBlock :: Block -> Heap (Maybe Block)
nextBlock b = readBlock (get offset b + blockSize b)

safeOffset :: Offset -> Heap a -> Heap (Maybe a)
safeOffset o c = do
  fs <- accessFile $ \h -> fromIntegral `liftM` hFileSize h
  if fs > o + headerSize then Just `liftM` c else return Nothing

unsafeReadHeader :: Offset -> Heap Header
unsafeReadHeader o =
  accessFile $ \h -> do
    hSeek h AbsoluteSeek (fromIntegral o)
    x <- read8 h
    s <- read32 h
    return (if (x :: Word8) == 0 then False else True, s)

unsafeReadSize :: Offset -> Heap Size
unsafeReadSize o =
  accessFile $ \h -> do
    hSeek h AbsoluteSeek (fromIntegral (o + 1))
    read32 h

readHeader :: Offset -> Heap (Maybe Header)
readHeader o = safeOffset o (unsafeReadHeader o)

unsafeReadBlock :: Offset -> Heap Block
unsafeReadBlock o = do
  (x, s) <- unsafeReadHeader o
  d <- accessFile $ \h -> if x
         then Just `liftM` B.hGet h s
         else return Nothing
  return (Block o s d)

readBlock :: Offset -> Heap (Maybe Block)
readBlock o = safeOffset o (unsafeReadBlock o)

writeHeader :: Offset -> Header -> Heap ()
writeHeader o (x, s) =
  accessFile $ \h ->
    do hSeek h AbsoluteSeek (fromIntegral o)
       write8  h (if x then (35::Word8) else 0)
       write32 h s

writePayload :: Offset -> Size -> B.ByteString -> Heap ()
writePayload o s d = do
  accessFile $ \h ->
    do hSeek h AbsoluteSeek (fromIntegral (o + headerSize))
       B.hPut h $ B.take (fromIntegral s) d

writeBlock :: Block -> Heap ()
writeBlock (Block o s d) = do
  writeHeader o (isJust d, s)
  maybe (return ())
    (\d' -> accessFile (\h -> B.hPut h $ B.take (fromIntegral s) d')) d

emptyHeap :: Handle -> FileHeap
emptyHeap h = FileHeap I.empty 0 h

insert :: Size -> Offset -> Heap ()
insert s o = modM allocMap (I.alter (f o) s)
  where f x Nothing   = Just [x]
        f x (Just xs) = Just (x:xs)

delete :: Size -> Heap ()
delete s = modM allocMap (I.update f s)
  where f (_:x:xs) = Just (x:xs)
        f _        = Nothing

grow, shrink :: Size -> Heap ()
grow   s = modM heapSize (+s)
shrink s = modM heapSize (-s+)

accessFile :: (Handle -> IO a) -> Heap a
accessFile c = getM file >>= liftIO . c

readAllocationMap :: Offset -> Heap ()
readAllocationMap o = do
  i <- readHeader o
  case i of
    Nothing -> return ()
    Just (f, s) -> do
      when (not f) (insert s o)
      grow (headerSize + s)
      readAllocationMap (o + headerSize + s)

findFreeBlock :: Size -> Heap (Maybe (Offset, Size))
findFreeBlock i = do
  a <- getM allocMap
  let mini = I.minViewWithKey $ snd $ I.split (i-1) a
  return $ case mini of
    Just ((s, o:_), _) -> Just (o, s)
    _                  -> Nothing

-- Basic operations. 

runHeap :: FilePath -> Heap a -> IO a
runHeap f c =
  withBinaryFile f ReadWriteMode $ \h ->
    fst `fmap` T.runStateT (readAllocationMap 0 >> c) (emptyHeap h)

allocate :: Size -> Heap Block
allocate i = do
  mb <- findFreeBlock i
  case mb of

    -- No free block found, allocate at end of heap.
    Nothing -> do
      e <- getM heapSize
      grow (headerSize + i)
      return $ Block e i Nothing

    -- Free block with sufficient size found, use this block.
    Just (o, s) -> do
      if s > headerSize + i + headerSize + splitThreshold
        then do
          delete s
          writeBlock $ Block (o + headerSize + i) (s - headerSize - i) Nothing
          insert (s - headerSize - i) (o + headerSize + i)
          return $ Block o i Nothing
        else do
          delete s
          return $ Block o s Nothing

free :: Offset -> Heap ()
free o = do
  t <- getM heapSize
  s <- accessFile $ \h -> do
    hSeek h AbsoluteSeek (fromIntegral o)
    write8 h (0::Int)
    read32 h
  if t /= o + headerSize + s
    then insert s o
    else do
      shrink (headerSize + s)
      accessFile (flip hSetFileSize (fromIntegral o))

read :: Offset -> Heap B.ByteString
read o = do
  p <- unsafeReadBlock o
  case get payload p of
    Nothing -> error "reading from unoccupied block"
    Just pl -> return pl

write :: B.ByteString -> Block -> Heap ()
write bs = writeBlock . set payload (Just bs)

-- Debug operations.

dumpAllocationMap :: Heap ()
dumpAllocationMap = T.get >>= liftIO . print

dump :: Heap ()
dump = readBlock 0 >>= f
  where
    f Nothing  = return ()
    f (Just b) = liftIO (print b) >> nextBlock b >>= f

