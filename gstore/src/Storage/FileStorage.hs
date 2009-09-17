module Storage.FileStorage
  ( Storage {- temp: -} (..)
  , Pointer {- temp: -} (..)
  , nullP

  , run
  , store
  , retrieve
  , delete
  , reuse
  , debug
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Binary
import Prelude hiding (read)
import Storage.FileHeap
import qualified Data.ByteString.Lazy as B

newtype Storage t a = S { unS :: Heap a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype Pointer (f :: * -> *) a = P { unP :: Int }
  deriving (Show, Binary)

nullP :: Pointer f a
nullP = P 0

run :: FilePath -> Storage t a -> IO a
run f = runHeap f . unS

store :: Binary (f a) => f a -> Storage t (Pointer f a)
store f = S $
  do let bs = encode f
     block <- allocate $ fromIntegral $ B.length bs
     write bs block
     return (P (location block))

retrieve :: Binary (f a) => Pointer f a -> Storage t (f a)
retrieve = S . liftM decode . read . unP

delete :: Pointer f a -> Storage t ()
delete = S . free . unP

reuse :: Binary (f a) => Pointer f a -> f a -> Storage t (Pointer f a)
reuse (P p) a = S $
  do s <- unsafeReadSize p
     writePayload p s (encode a)
     return (P p)

debug :: Storage t ()
debug = S (dump >> dumpAllocationMap)

