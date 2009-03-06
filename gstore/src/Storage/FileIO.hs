module Storage.FileIO where

import Control.Monad
import Data.Char
import Data.Bits
import System.IO

infixl 1 <<

(<<) :: Monad m => m b -> m a -> m b
b << a = a >> b

-- Read and write fixed bit integers values (in little-endian way?).
-- Larger integers will be truncated accordingly.

read8, read16, read32 :: Integral a => Handle -> IO a

read8  h = (fromIntegral . ord) `liftM` hGetChar h
read16 h = liftM2 (+) (liftM (*0xFF)   (read8  h)) (read8  h)  
read32 h = liftM2 (+) (liftM (*0xFFFF) (read16 h)) (read16 h)

write8, write16, write32 :: (Bits a, Integral a) => Handle -> a -> IO ()

write8  h   = hPutChar h . chr . fromIntegral
write16 h i = write8  h i << write8  h (i `shiftR` 8)
write32 h i = write16 h i << write16 h (i `shiftR` 16)
 
