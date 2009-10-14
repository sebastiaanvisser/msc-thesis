module Storage.FileStorage
--   ( Storage {- temp: -} (..)
--   , Pointer {- temp: -} (..)
--   , nullP
-- 
--   , run
--   , store
--   , retrieve
--   , delete
--   , reuse
--   , debug
--   )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Maybe
import Data.Binary
import Prelude hiding (read)
import Storage.FileHeap
import qualified Data.ByteString.Lazy as B

newtype Storage t a = Storage { unStorage :: HeapRW a }
  deriving (Functor, Applicative, Monad) 

newtype Pointer (f :: * -> *) a = Ptr { unPtr :: Int }
  deriving (Show, Binary)

nullP :: Pointer f a
nullP = Ptr 0

run :: FilePath -> Storage t a -> IO a
run f = runHeap f . unStorage

retrieve :: Binary (f a) => Pointer f a -> Storage t (f a)
retrieve = Storage . liftM (decode . fm) . read . unPtr
  where fm = fromMaybe (error "retrieve failed")

{-
store :: Binary (f a) => f a -> Storage t (Pointer f a)
store f = Storage $
  do let bs = encode f
     block <- allocate $ fromIntegral $ B.length bs
     write bs block
     return (Ptr (location block))

delete :: Pointer f a -> Storage t ()
delete = Storage . free . unP

reuse :: Binary (f a) => Pointer f a -> f a -> Storage t (Pointer f a)
reuse (Ptr p) a = Storage $
  do s <- unsafeReadSize p
     writePayload p s (encode a)
     return (Ptr p)

debug :: Storage t ()
debug = Storage (dump >> dumpAllocationMap)
-}
