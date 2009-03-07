module Storage.Storage (

    Storage {- temp: -} (..)
  , Pointer {- temp: -} (..)
  , nullP

  , run
  , store
  , retrieve
  , delete
  , reuse
  , debug

  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Binary
import Prelude hiding (read)
import Storage.FileHeap
import qualified Data.ByteString.Lazy as B

newtype Storage t  a = S { unS :: Heap a }
newtype Pointer a = P { unP :: Int }

nullP :: Pointer a
nullP = P 0

instance Show (Pointer a) where
  show (P p) = "P:" ++ show p

instance Binary (Pointer a) where
  get = P `fmap` get
  put = put . unP

instance Monad (Storage t) where
  a >>= b  = S (unS a >>= (unS . b))
  return a = S (return a)

instance Functor (Storage t) where
  fmap f (S s) = S (fmap f s)

instance Applicative (Storage t) where
  pure  = return
  (<*>) = ap

instance MonadIO (Storage t) where
  liftIO c = S (liftIO c)

run      :: FilePath -> Storage t a -> IO a
store    :: Binary a => a -> Storage t (Pointer a)
retrieve :: Binary a => Pointer a -> Storage t a
delete   :: Pointer a -> Storage t ()
reuse    :: Binary a => Pointer a -> a -> Storage t (Pointer a)
debug    :: Storage t ()

-- Implementations.

run f = runHeap f . unS

store a = S $
  do let bs = encode a
     block <- allocate $ fromIntegral $ B.length bs
     write bs block
     return $ P (location block)

retrieve = S . liftM decode . read . unP

delete = S . free . unP

reuse (P p) a = S $
  do s <- unsafeReadSize p
     writePayload p s (encode a)
     return (P p)

debug = S $ dump >> dumpAllocationMap

