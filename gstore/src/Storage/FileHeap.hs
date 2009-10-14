{-# LANGUAGE TemplateHaskell #-}
module Storage.FileHeap
{-  ( Heap
  , runHeap
  , location

  , unsafeReadSize
  , writePayload

  , allocate
  , free
  , write
  , read
  , read'

  , dump
  , dumpAllocationMap
  )-}
where

import Control.Applicative
import Data.Binary
import Data.Record.Label
import Data.Word
import Data.Maybe
import Prelude hiding (read)
import Storage.FileIO
import System.IO
import System.IO.Lazy
import qualified Control.Monad.Reader as Rd
import qualified Control.Monad.State as St
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as I
import Control.Monad
import Control.Monad.Trans

-- Data types.
type Offset  = Int
type Size    = Int
type Header  = (Bool, Size)

data Block =
  Block {
    _offset  :: Offset
  , _size    :: Size
  , _payload :: Maybe B.ByteString
  } deriving (Eq, Ord, Show)

data FileHeap = 
  FileHeap {
    _allocMap :: I.IntMap [Int]
  , _heapSize :: Int
  , _file     :: Handle
  } deriving Show

$(mkLabels [''Block, ''FileHeap])

data Par m n a = Par { runA :: m a, runB :: n a }

newtype HeapRO a = HeapRO { runRO :: Rd.ReaderT Handle Lazy a }
  deriving (Functor, Applicative, Monad, MonadIO, Rd.MonadReader Handle)

newtype HeapRW a = HeapRW { runRW :: St.StateT FileHeap HeapRO a }
  deriving (Functor, Applicative, Monad, MonadIO, St.MonadState FileHeap)

newtype Pointer (f :: * -> *) a = Ptr { unPtr :: Offset }
  deriving (Show, Binary)

file     :: FileHeap :-> Handle
heapSize :: FileHeap :-> Size
allocMap :: FileHeap :-> I.IntMap [Int]
payload  :: Block :-> Maybe B.ByteString
size     :: Block :-> Size
offset   :: Block :-> Offset

------------------ TOP OPS ---------------------------

nullP :: Pointer f a
nullP = Ptr 0

retrieve :: Binary (f a) => Pointer f a -> HeapRO (f a)
retrieve (Ptr p) = decode . fm <$> read p
  where fm = fromMaybe (error "retrieve failed")

------------------ READ OPS --------------------------

headerSize :: Size
headerSize = 1 -- Occupied flag
           + 4 -- Block size

read :: Offset -> HeapRO (Maybe B.ByteString)
read o =
  do h <- Rd.ask
     liftIO $
       do fs <- fromIntegral <$> hFileSize h
          if fs <= o + headerSize
            then return Nothing
            else 
              do hSeek h AbsoluteSeek (fromIntegral o)
                 x <- read8 h  :: IO Word8
                 s <- read32 h
                 let c = x /= 0
                 d <- if c
                      then Just <$> B.hGet h s
                      else return Nothing
                 return d

------------------ ALLOC OPS -------------------------

grow :: Size -> HeapRW ()
grow s = modM heapSize (+s)

shrink :: Size -> HeapRW ()
shrink s = modM heapSize (-s+)

findFreeBlock :: Size -> HeapRW (Maybe (Offset, Size))
findFreeBlock i =
  do a <- getM allocMap
     return $
       do ((s, o:_), _) <- I.minViewWithKey (snd (I.split (i-1) a))
          return (o, s)

{-allocate :: Size -> Heap Block
allocate i =
  do mb <- findFreeBlock i
     case mb of

       -- No free block found, allocate at end of heap.
       Nothing -> do
         do e <- getM heapSize
            grow (headerSize + i)
            return (Block e i Nothing)

       -- Free block with sufficient size found, use this block.
       Just (o, s) ->
         if s > headerSize + i + headerSize + splitThreshold
           then
             do delete s
                writeBlock (Block (o + headerSize + i) (s - headerSize - i) Nothing)
                insert (s - headerSize - i) (o + headerSize + i)
                return (Block o i Nothing)
           else
             do delete s
                return (Block o s Nothing)
-}

free :: Offset -> HeapRW ()
free o =
  do t <- getM heapSize
     h <- lazy Rd.ask
     s <- liftIO $
       do hSeek h AbsoluteSeek (fromIntegral o)
          write8 h (0::Int)
          read32 h
     if t /= o + headerSize + s
       then insert s o
       else
         do shrink (headerSize + s)
            liftIO (hSetFileSize h (fromIntegral o))

insert :: Int -> Int -> HeapRW ()
insert s o = modM allocMap (I.alter (f o) s)
  where f x Nothing   = Just [x]
        f x (Just xs) = Just (x:xs)

delete :: Int -> HeapRW ()
delete s = modM allocMap (I.update f s)
  where f (_:x:xs) = Just (x:xs)
        f _        = Nothing

------------------ WRITE OPS -------------------------

-- writeHeader :: Offset -> Header -> HeapRW ()
-- writeHeader o (x, s) =
--   h <- lift $ Rd.ask
--     do hSeek h AbsoluteSeek (fromIntegral o)
--        write8  h (if x then (35::Word8) else 0)
--        write32 h s

-- writeBlock :: B.ByteString -> Block -> HeapRW ()
-- writeBlock bs (Block o s d) =
--   do writeHeader o (isJust d, s)
--      maybe (return ())
--        (\d' -> accessFileStrict (\h -> B.hPut h (B.take (fromIntegral s) d'))) d

-- write :: B.ByteString -> Block -> HeapRW ()
-- write bs = writeBlock . set payload (Just bs)

----------------- RUN OPS -----------------------------

lazy :: HeapRO a -> HeapRW a
lazy = HeapRW . lift

runHeap :: FilePath -> HeapRW a -> IO a
runHeap f c =
  do h <- openBinaryFile f ReadWriteMode
     let runner =
             runLazy
           . flip Rd.runReaderT h
           . runRO
           . flip St.evalStateT (emptyHeap h)
           . runRW
     runner c
  where emptyHeap h = FileHeap I.empty 0 h
--(readAllocationMap 0 >> c)




{--
-- Helper functions.

location :: Block -> Offset
location = get offset

splitThreshold :: Size
splitThreshold = 4

blockSize :: Block -> Size
blockSize = (+headerSize) . get size

nextBlock :: Block -> Heap (Maybe Block)
nextBlock b = readBlock (get offset b + blockSize b)

----------------------------------------

safeOffset :: Offset -> Heap a -> Heap (Maybe a)
safeOffset o c =
  do fs <- accessFileLazy (liftM fromIntegral . hFileSize)
     if fs > o + headerSize then Just `liftM` c else return Nothing

unsafeReadHeader :: Offset -> Heap Header
unsafeReadHeader o =
  accessFileLazy $ \h ->
    do hSeek h AbsoluteSeek (fromIntegral o)
       x <- read8 h
       s <- read32 h
       return (if (x :: Word8) == 0 then False else True, s)

unsafeReadSize :: Offset -> Heap Size
unsafeReadSize o =
  accessFileLazy $ \h ->
    do hSeek h AbsoluteSeek (fromIntegral (o + 1))
       read32 h

readHeader :: Offset -> Heap (Maybe Header)
readHeader o = safeOffset o (unsafeReadHeader o)

unsafeReadBlock :: Offset -> Heap Block
unsafeReadBlock o =
  do (x, s) <- unsafeReadHeader o
     d <- accessFileLazy $ \h ->
            if x
            then Just `liftM` B.hGet h s
            else return Nothing
     return (Block o s d)

readBlock :: Offset -> Heap (Maybe Block)
readBlock o = safeOffset o (unsafeReadBlock o)

read :: Offset -> Heap B.ByteString
read o =
  do p <- unsafeReadBlock o
     case get payload p of
       Nothing -> error "reading from unoccupied block"
       Just pl -> return pl

read' :: Offset -> Heap B.ByteString
read' o =
  accessFileLazy $ \h ->
    do putStr "." >> hFlush stdout
       hSeek h AbsoluteSeek (fromIntegral o)
       read8 h :: IO Word8
       s <- read32 h
       B.hGet h s

accessFileLazy :: (Handle -> IO a) -> Heap a
accessFileLazy c = getM file >>= lift . Lazy . c

----------------------------------------

writeHeader :: Offset -> Header -> Heap ()
writeHeader o (x, s) =
  accessFileStrict $ \h ->
    do hSeek h AbsoluteSeek (fromIntegral o)
       write8  h (if x then (35::Word8) else 0)
       write32 h s

writePayload :: Offset -> Size -> B.ByteString -> Heap ()
writePayload o s d =
  accessFileStrict $ \h ->
    do hSeek h AbsoluteSeek (fromIntegral (o + headerSize))
       B.hPut h (B.take (fromIntegral s) d)

writeBlock :: Block -> Heap ()
writeBlock (Block o s d) =
  do writeHeader o (isJust d, s)
     maybe (return ())
       (\d' -> accessFileStrict (\h -> B.hPut h (B.take (fromIntegral s) d'))) d

emptyHeap :: Handle -> FileHeap
emptyHeap h = FileHeap I.empty 0 h

----------------------------------------

accessFileStrict :: (Handle -> IO a) -> Heap a
accessFileStrict c = getM file >>= lift . liftIO . c

readAllocationMap :: Offset -> Heap ()
readAllocationMap o =
  do i <- readHeader o
     case i of
       Just (f, s) ->
         do when (not f) (insert s o)
            grow (headerSize + s)
            readAllocationMap (o + headerSize + s)
       Nothing -> return ()

-- Basic operations. 

runHeap :: FilePath -> Heap a -> IO a
runHeap f c =
  do h <- openBinaryFile f ReadWriteMode
    --   withBinaryFile f ReadWriteMode $ \h ->
     run $ liftM fst $ flip St.runStateT (emptyHeap h)
         (readAllocationMap 0 >> c)

allocate :: Size -> Heap Block
allocate i =
  do mb <- findFreeBlock i
     case mb of

       -- No free block found, allocate at end of heap.
       Nothing -> do
         do e <- getM heapSize
            grow (headerSize + i)
            return (Block e i Nothing)

       -- Free block with sufficient size found, use this block.
       Just (o, s) ->
         if s > headerSize + i + headerSize + splitThreshold
           then
             do delete s
                writeBlock (Block (o + headerSize + i) (s - headerSize - i) Nothing)
                insert (s - headerSize - i) (o + headerSize + i)
                return (Block o i Nothing)
           else
             do delete s
                return (Block o s Nothing)

free :: Offset -> Heap ()
free o =
  do t <- getM heapSize
     s <- accessFileStrict $ \h ->
       do hSeek h AbsoluteSeek (fromIntegral o)
          write8 h (0::Int)
          read32 h
     if t /= o + headerSize + s
       then insert s o
       else
         do shrink (headerSize + s)
            accessFileStrict (flip hSetFileSize (fromIntegral o))

write :: B.ByteString -> Block -> Heap ()
write bs = writeBlock . set payload (Just bs)

-- Debug operations.

dumpAllocationMap :: Heap ()
dumpAllocationMap = St.get >>= lift . liftIO . print

dump :: Heap ()
dump = readBlock 0 >>= f
  where
    f Nothing  = return ()
    f (Just b) = lift (liftIO (print b)) >> nextBlock b >>= f
-}
