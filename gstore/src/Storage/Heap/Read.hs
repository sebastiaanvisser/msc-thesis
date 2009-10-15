module Storage.Heap.Read where

import Control.Applicative
import Control.Monad.Lazy
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Binary
import Data.ByteString.Lazy
import Data.Maybe
import Data.Word
import Prelude hiding (read)
import Storage.FileIO
import Storage.Heap.Block
import System.IO

newtype Heap a = Heap (ReaderT Handle IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Handle
    , Lazy
    )

run :: Handle -> Heap a -> IO a
run h (Heap c) = runReaderT c h

retrieve :: Binary (f a) => Pointer f a -> Heap (f a)
retrieve (Ptr p) = decode . fm <$> read p
  where fm = fromMaybe (error "retrieve failed")

safeOffset :: Offset -> Heap a -> Heap (Maybe a)
safeOffset o c =
  do h <- ask
     fs <- liftIO (hFileSize h)
     if fromIntegral fs > o + headerSize
       then Just <$> c
       else return Nothing

unsafeReadHeader :: Offset -> Heap Header
unsafeReadHeader o =
  do h <- ask
     liftIO $
       do hSeek h AbsoluteSeek (fromIntegral o)
          x <- read8 h
          s <- read32 h
          return (if (x :: Word8) == 0 then False else True, s)

readHeader :: Offset -> Heap (Maybe Header)
readHeader o = safeOffset o (unsafeReadHeader o)

read :: Offset -> Heap (Maybe ByteString)
read o =
  do h <- ask
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
                      then Just <$> hGet h s
                      else return Nothing
                 return d

