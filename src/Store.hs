module Store where

import Control.Monad
import Data.Maybe (isJust)
import Data.Word
import IO
import System.IO
import qualified Data.ByteString as B
import qualified Data.IntMap as M

-------------------------------------------------------------------------------

type Offset = Int
type Size   = Int
type Header = (Bool, Size)

data Block =
  Block {
    offset   :: Offset
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

safeOffset :: Handle -> Offset -> IO a -> IO (Maybe a)
safeOffset h o c = do
  fs <- fromIntegral `liftM` hFileSize h
  if fs > o then Just `liftM` c else return Nothing

unsafeReadHeader :: Handle -> Offset -> IO Header
unsafeReadHeader h o = do
  hSeek h AbsoluteSeek (fromIntegral o)
  x <- read8 h
  s <- read32 h
  return (int2bool x, s)

readHeader :: Handle -> Offset -> IO (Maybe Header)
readHeader h o = safeOffset h o (unsafeReadHeader h o)

unsafeReadBlock :: Handle -> Offset -> IO Block
unsafeReadBlock h o = do
  (x, s) <- unsafeReadHeader h o
  d <- if x
       then Just `liftM` B.hGet h s
       else return Nothing
  return (Block o s d)

readBlock :: Handle -> Offset -> IO (Maybe Block)
readBlock h o = safeOffset h o (unsafeReadBlock h o)

writeHeader :: Handle -> Offset -> Header -> IO ()
writeHeader h o (x, s) = do
  hSeek h AbsoluteSeek (fromIntegral o)
  write8  h (bool2int x)
  write32 h s

writeBlock :: Handle -> Block -> IO ()
writeBlock h (Block o s d) = do
  writeHeader h o (isJust d, s)
  maybe (return ()) (B.hPut h . B.take s) d

-------- block allocation and releasing ---------------------------------------

type AllocationMap = M.IntMap Int

readAllocationMap :: Handle -> Offset -> AllocationMap -> IO AllocationMap
readAllocationMap h o m = do
  i <- readHeader h o
  case i of
    Just (False, s) -> readAllocationMap h (o + headerSize + s) (M.insert o s m)
    Just (True,  s) -> readAllocationMap h (o + headerSize + s) m
    Nothing         -> return m 

allocate :: Handle -> Offset -> Size ->  IO (Maybe Block)
allocate h o a = do
  i <- readHeader h o
  case i of
    Nothing                  -> return $ Just (Block o a Nothing) -- eof
    Just (False, s) | s >= a -> readBlock h o
    Just (_,     s)          -> allocate h (o + headerSize + s) a

free :: Handle -> Block -> IO ()
free h b = writeBlock h (b {payload = Nothing})

fillBlock :: Word8 -> Block -> Block
fillBlock w b = b { payload = Just (B.replicate (size b) w) }

splitBlock :: Handle -> Size -> Block -> IO ()
splitBlock h n (Block o s d) = do
  let a = Block o n Nothing
      b = Block (o + headerSize + n) (s - n - headerSize) Nothing
  writeBlock h a
  writeBlock h b

nextBlock :: Handle -> Block -> IO (Maybe Block)
nextBlock h b = readBlock h (offset b + blockSize b)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  h <- openBinaryFile "test.db" ReadWriteMode
  hSetFileSize h 0
  writeBlock h (Block 0 0 Nothing)
  allocate h 0 4 >>= maybe (return ()) (writeBlock h . fillBlock 0x34)
  allocate h 0 6 >>= maybe (return ()) (writeBlock h . fillBlock 0x35)
  allocate h 0 7 >>= maybe (return ()) (writeBlock h . fillBlock 0x36)
  allocate h 0 9 >>= maybe (return ()) (writeBlock h . fillBlock 0x37)
  allocate h 0 3 >>= maybe (return ()) (writeBlock h . fillBlock 0x39)
  allocate h 0 8 >>= maybe (return ()) (writeBlock h . fillBlock 0x38)
  readBlock h 25 >>= maybe (return ()) (free h)
  readBlock h 51 >>= maybe (return ()) (free h)
  readBlock h 0 >>= printBlocks h

  am <- readAllocationMap h 0 M.empty
  print am


printBlocks h Nothing  = return ()
printBlocks h (Just b) = print b >> nextBlock h b >>= printBlocks h

