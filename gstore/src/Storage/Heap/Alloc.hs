{-# LANGUAGE TemplateHaskell #-}
module Storage.Heap.Alloc where

import Control.Applicative
import Control.Monad.Lazy
import Control.Monad.Reader
import Control.Monad.State
import Data.Record.Label
import Data.Word
import Storage.FileIO
import Storage.Heap.Block 
import Storage.Heap.Read hiding (Heap, run)
import System.IO
import qualified Data.IntMap as I
import qualified Storage.Heap.Read as R

type AllocationMap = I.IntMap [Int]

data FileHeap = 
  FileHeap
  { _allocMap :: AllocationMap
  , _heapSize :: Int
  , _file     :: Handle
  } deriving Show

$(mkLabels [''FileHeap])

newtype Heap a = Heap { run' :: StateT FileHeap R.Heap a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState FileHeap
    , MonadReader Handle
    )

instance LiftLazy R.Heap Heap where
  liftLazy = Heap . lift

file     :: FileHeap :-> Handle
heapSize :: FileHeap :-> Size
allocMap :: FileHeap :-> I.IntMap [Int]

-- Exported functions.

run :: Handle -> Heap a -> R.Heap a
run h c = evalStateT
  (run' $
    do readAllocationMap 0
       m <- getM allocMap
       liftIO (print m)
       c
  )
  (FileHeap I.empty 0 h)

allocate :: Size -> Heap Block
allocate i =
  findFreeBlock i >>= maybe
    (allocateAtEnd i)  -- No free block found, allocate at end of heap.
    (reuseFreeBlock i) -- Free block with sufficient size found, use this block.

free :: Offset -> Heap ()
free o =
  do t <- getM heapSize
     h <- ask
     s <- liftIO $
       do hSeek h AbsoluteSeek (fromIntegral o)
          write8 h (0::Int)
          read32 h
     if t /= o + headerSize + s
       then mkFree o s
       else
         do shrink (headerSize + s)
            liftIO (hSetFileSize h (fromIntegral o))

-- Helper functions.

grow :: Size -> Heap ()
grow s = modM heapSize (+s)

shrink :: Size -> Heap ()
shrink s = modM heapSize (-s+)

findFreeBlock :: Size -> Heap (Maybe (Offset, Size))
findFreeBlock i = f <$> getM allocMap
  where f = fmap (swp . fmap head . fst) . I.minViewWithKey . snd . I.split (i - 1)
        swp (a, b) = (b, a)

reuseFreeBlock :: Size -> (Size, Size) -> Heap Block
reuseFreeBlock i (o, s) =
  do unFree s
     if s > headerSize + i + headerSize + splitThreshold
       then splitAndUse i o s
       else return (Block o s Nothing)

unFree :: Int -> Heap ()
unFree s = modM allocMap (I.update f s)
  where f (_:x:xs) = Just (x:xs)
        f _        = Nothing

mkFree :: Offset -> Size -> Heap ()
mkFree o = modM allocMap . I.alter (Just . maybe [o] (o:))

splitAndUse :: Size -> Offset -> Size -> Heap Block
splitAndUse i o s =
  do let o' = o + headerSize + i
         s' = s - headerSize - i
     splitter o' s'
     mkFree   o' s'
     return (Block o i Nothing)

splitter :: Offset -> Size -> Heap ()
splitter o s =
  do h <- ask
     liftIO $
       do hSeek h AbsoluteSeek (fromIntegral o)
          write8  h (0x00 :: Word8)
          write32 h s

allocateAtEnd :: Size -> Heap Block
allocateAtEnd i =
  do e <- getM heapSize
     grow (headerSize + i)
     return (Block e i Nothing)

readAllocationMap :: Offset -> Heap ()
readAllocationMap o =
  do i <- Heap . lift $ readHeader o
     case i of
       Just (f, s) ->
         do when (not f) (mkFree o s)
            grow (headerSize + s)
            readAllocationMap (o + headerSize + s)
       Nothing -> return ()

