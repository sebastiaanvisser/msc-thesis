{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Store where

import Control.Monad.State
import Control.Monad
import Data.Maybe
import Data.Record.Label
import Data.Word
import IO
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.IntMap as I

-------------------------------------------------------------------------------

type Pointer = Int
type Size    = Int
type Header  = (Bool, Size)

data Block =
  Block {
    offset   :: Pointer
  , size     :: Size
  , payload  :: Maybe B.ByteString
  } deriving (Eq, Ord, Show)

-------- helper functions -----------------------------------------------------

int2bool :: Int -> Bool
int2bool 0 = False
int2bool _ = True

bool2int :: Bool -> Int
bool2int False = 0
bool2int True  = 1

-------- primitive block IO ---------------------------------------------------

headerSize :: Size
headerSize = 1 + 4

blockSize :: Block -> Size
blockSize = (+headerSize) . size

-- TODO handles to the last argument for better currying!

safeOffset :: Handle -> Pointer -> IO a -> IO (Maybe a)
safeOffset h o c = do
  fs <- fromIntegral `liftM` hFileSize h
  if fs > o then Just `liftM` c else return Nothing

unsafeReadHeader :: Handle -> Pointer -> IO Header
unsafeReadHeader h o = do
  hSeek h AbsoluteSeek (fromIntegral o)
  x <- read8 h
  s <- read32 h
  return (int2bool x, s)

readHeader :: Pointer -> Handle -> IO (Maybe Header)
readHeader o h = safeOffset h o (unsafeReadHeader h o)

unsafeReadBlock :: Handle -> Pointer -> IO Block
unsafeReadBlock h o = do
  (x, s) <- unsafeReadHeader h o
  d <- if x
       then Just `liftM` B.hGet h s
       else return Nothing
  return (Block o s d)

readBlock :: Handle -> Pointer -> IO (Maybe Block)
readBlock h o = safeOffset h o (unsafeReadBlock h o)

writeHeader :: Handle -> Pointer -> Header -> IO ()
writeHeader h o (x, s) = do
  hSeek h AbsoluteSeek (fromIntegral o)
  write8  h (bool2int x)
  write32 h s

writeBlock :: Handle -> Block -> IO ()
writeBlock h (Block o s d) = do
  writeHeader h o (isJust d, s)
  maybe (return ()) (B.hPut h . B.take s) d

-------- block allocation and releasing ---------------------------------------

data AbsHeap = 
  AbsHeap {
    _allocMap :: I.IntMap [Int]
  , _heapSize :: Int
  , _file     :: Handle
  } deriving Show

$(mkLabels [''AbsHeap])

emptyHeap :: Handle -> AbsHeap
emptyHeap h = AbsHeap I.empty 0 h

-- statefy these
insert :: Size -> Pointer -> Heap ()
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

type Heap m = StateT AbsHeap IO m

accessFile :: (Handle -> IO a) -> Heap a
accessFile c = getM file >>= liftIO . c

runHeap :: FilePath -> Heap a -> IO a
runHeap f c = do
  h <- openBinaryFile f ReadWriteMode
  (a, _) <- runStateT (represent 0 >> c) (emptyHeap h)
  return a

represent :: Pointer -> Heap ()
represent o = do
  i <- accessFile (readHeader o)
  case i of
    Nothing -> return ()
    Just (f, s) -> do
      when (not f) (insert s o)
      grow (headerSize + s)
      represent (o + headerSize + s)

findFreeBlock :: Size -> Heap (Maybe (Pointer, Size))
findFreeBlock i = do
  a <- getM allocMap
  let min = I.minViewWithKey $ snd $ I.split (i-1) a
  return $ case min of
    Just ((s, o:xs), _) -> Just (o, s)
    _                   -> Nothing

allocate :: Size -> Heap Block
allocate i = do
  mb <- findFreeBlock i
  case mb of

    -- No free block found, allocate at end of heap.
    Nothing -> do
      e <- getM heapSize
      grow (headerSize + i)
      return $ Block e i Nothing

    -- Free block with sufficient size found, reuse this block.
    Just h@(o, s) -> do
      delete s
      return $ Block o s Nothing

free :: Pointer -> Heap ()
free o = do
  s <- accessFile $ \h -> do
    hSeek h AbsoluteSeek (fromIntegral o)
    write8 h (0::Int)
    read32 h
  insert s o

dump :: Heap ()
dump = do
  liftIO $ putStrLn "Heap contents:"
  accessFile (\h -> readBlock h 0 >>= printBlocks h)

fillBlock :: Word8 -> Block -> Block
fillBlock w b = b { payload = Just (B.replicate (size b) w) }

nextBlock :: Handle -> Block -> IO (Maybe Block)
nextBlock h b = readBlock h (offset b + blockSize b)

printBlocks :: Handle -> Maybe Block -> IO ()
printBlocks h Nothing  = return ()
printBlocks h (Just b) = print b >> nextBlock h b >>= printBlocks h

-------------------------------------------------------------------------------

storeString :: String -> Heap ()
storeString s = do
  let bs = U.fromString s
  b <- allocate (B.length bs)
  accessFile $ \h -> writeBlock h (b { payload = Just bs })

main :: IO ()
main = do
  runHeap "test.db" $ do
    get >>= liftIO . print 
    dump
    storeString "hallo!"
    storeString "zomaar viel spaß jö!"
    storeString "haa"
    get >>= liftIO . print 
    dump
  return ()

