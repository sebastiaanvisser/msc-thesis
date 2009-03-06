module Storage.Storage (

    Storage    {- temp: -} (..)
  , Persistent {- temp: -} (..)
  , nullP

  , run
  , store
  , retrieve
  , delete
  , reuse
  , debug

  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Binary
import Prelude hiding (read)
import Storage.FileHeap
import qualified Data.ByteString.Lazy as B

newtype Storage t  a = S { unS :: Heap a }
newtype Persistent a = P { unP :: Int }

nullP :: Persistent a
nullP = P 0

instance Show (Persistent a) where
  show (P p) = "P:" ++ show p

instance Binary (Persistent a) where
  get = P `fmap` get
  put = put . unP

instance Monad (Storage t) where
  a >>= b  = S (unS a >>= (unS . b))
  return a = S (return a)

instance MonadIO (Storage t) where
  liftIO c = S (liftIO c)

run      :: FilePath -> Storage t a -> IO a
store    :: Binary a => a -> Storage t (Persistent a)
retrieve :: Binary a => Persistent a -> Storage t a
delete   :: Persistent a -> Storage t ()
reuse    :: Binary a => Persistent a -> a -> Storage t ()
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
     return ()

debug = S $ dump

