{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Heap.FileHeap (
    Heap
  , runHeap
  , initHeap
  , dumpHeap
  , dumpAllocationMap

  , Block
  , block
  , pointer

  , allocate
  , free
  , write
  , read
  ) where

import Prelude hiding (read)
import Control.Monad.State
import Data.Int
import Data.Maybe
import Data.Record.Label
import Heap.FileIO
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as I

type Offset  = Int
type Size    = Int
type Header  = (Bool, Size)

data Pointer =
    PtrDirect Offset
  | PtrBlock  Block

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

$(mkLabels [''Block, ''FileHeap])

file     :: Label FileHeap Handle
heapSize :: Label FileHeap Int
allocMap :: Label FileHeap (I.IntMap [Int])
payload  :: Label Block (Maybe B.ByteString)
size     :: Label Block Size
offset   :: Label Block Offset

type Heap m = StateT FileHeap IO m

pointer :: Block -> Offset
pointer = lget offset

int2bool :: Int -> Bool
int2bool 0 = False
int2bool _ = True

bool2int :: Bool -> Int
bool2int False = 0
bool2int True  = 1

headerSize :: Size
headerSize = 1 + 4

blockSize :: Block -> Size
blockSize = (+headerSize) . lget size

nextBlock :: Block -> Heap (Maybe Block)
nextBlock b = readBlock (lget offset b + blockSize b)

dumpHeap :: Heap ()
dumpHeap = readBlock 0 >>= f
  where
    f Nothing  = return ()
    f (Just b) = liftIO (print b) >> nextBlock b >>= f

safeOffset :: Offset -> Heap a -> Heap (Maybe a)
safeOffset o c = do
  fs <- accessFile $ \h -> fromIntegral `liftM` hFileSize h
  if fs > o then Just `liftM` c else return Nothing

unsafeReadHeader :: Offset -> Heap Header
unsafeReadHeader o =
  accessFile $ \h -> do
    hSeek h AbsoluteSeek (fromIntegral o)
    x <- read8 h
    s <- read32 h
    return (int2bool x, s)

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

block :: Offset -> Heap Block
block = unsafeReadBlock

writeHeader :: Offset -> Header -> Heap ()
writeHeader o (x, s) =
  accessFile $ \h -> do
    hSeek h AbsoluteSeek (fromIntegral o)
    write8  h (bool2int x)
    write32 h s

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

grow, shrink :: Int -> Heap ()
grow   s = modM heapSize (+s)
shrink s = modM heapSize (-s+)

accessFile :: (Handle -> IO a) -> Heap a
accessFile c = getM file >>= liftIO . c

runHeap :: FilePath -> Heap a -> IO a
runHeap f c = do
  h <- openBinaryFile f ReadWriteMode
  (a, _) <- runStateT (readAllocationMap 0 >> c) (emptyHeap h)
  return a

initHeap :: Heap ()
initHeap = do
  accessFile (\h -> hSetFileSize h 0)
  writeBlock $ Block 0 0 Nothing

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

dumpAllocationMap :: Heap ()
dumpAllocationMap = get >>= liftIO . print

splitThreshold :: Int
splitThreshold = 4

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
          liftIO (print i)
          delete s
          writeBlock $ Block (o + headerSize + i) (s - headerSize - i) Nothing
          insert (s - headerSize - i) (o + headerSize + i)
          return $ Block o i Nothing
        else do
          delete s
          return $ Block o s Nothing

free :: Block -> Heap ()
free p = do
  let o = lget offset p
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
  case lget payload p of
    Nothing -> error "reading from unoccupied block"
    Just pl -> return pl

write :: B.ByteString -> Block -> Heap ()
write bs b = writeBlock (lset payload (Just bs) b)

