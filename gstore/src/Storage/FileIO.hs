module Storage.FileIO where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import System.IO

infixl 1 <<

(<<) :: Monad m => m b -> m a -> m b
b << a = a >> b

-- Read and write fixed bit integers values (in little-endian way?).
-- Larger integers will be truncated accordingly.

read8, read16, read32 :: Integral a => Handle -> IO a

read8  h = fromIntegral . ord <$> hGetChar h
read16 h = (\a b -> a * 0x100   + b) <$> read8  h <*> read8  h
read32 h = (\a b -> a * 0x10000 + b) <$> read16 h <*> read16 h

write8, write16, write32 :: (Bits a, Integral a) => Handle -> a -> IO ()

write8  h   = hPutChar h . chr . (.&. 0xFF) . fromIntegral
write16 h i = write8  h i << write8  h (i `shiftR` 8)
write32 h i = write16 h i << write16 h (i `shiftR` 16)
 
