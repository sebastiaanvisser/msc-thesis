{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Storage.Storage where

import Codec.Compression.GZip
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Binary
import qualified Data.ByteString.Lazy as B

newtype Storage a = S { unS :: a }
newtype Pointer a = P { unP :: a }

-- instance Show (Pointer a) where
--   show (P p) = "P:" ++ show p

-- instance Binary (Pointer a) where
--   get = P `fmap` get
--   put = put . unP

instance Monad Storage where
  a >>= b  = undefined -- S (unS a >>= (unS . b))
  return a = undefined -- S (return a)

instance Functor Pointer where
  fmap f (P s) = P (f s)

{-instance Functor Storage where
  fmap f (S s) = S (fmap f s)

instance Applicative Storage where
  pure  = return
  (<*>) = ap

instance MonadIO Storage where
  liftIO c = S (liftIO c)
-} 

data Store m a = forall b.
  Store {
    store    :: b -> m (Pointer b)
  , retrieve :: Pointer b -> m b
  }

mkFileStore :: FilePath -> Store Storage B.ByteString
mkFileStore _ = Store stBin rtBin

stBin :: B.ByteString -> Storage (Pointer B.ByteString)
stBin = undefined

rtBin :: Pointer B.ByteString -> Storage B.ByteString
rtBin = undefined




mkProfilingStore :: Monad m => Store m a -> Store (StateT Int m) a
mkProfilingStore (Store s r) = Store
  (\k -> do v <- lift (s k) ; modify (+1) ; return v)
  (\k -> do v <- lift (r k) ; modify (+1) ; return v)

mkEncodedStore :: (Monad m, Binary a) => Store m B.ByteString -> Store m a
mkEncodedStore (Store s r) = Store
  (\v -> s (     encode v) >>= return . fmap decode)
  (\v -> r (fmap encode v) >>= return .      decode)

-- mkCompressedStore :: Monad m => Store m B.ByteString -> Store m B.ByteString
-- mkCompressedStore (Store s r) = Store
--   (\v -> s (compress v))
--   (\v -> r v >>= return . decompress)

-- myStorage :: Store (StateT Int Storage) B.ByteString
-- myStorage = 
--     mkProfilingStore
--   $ mkCompressedStore
--   $ mkEncodedStore
--   $ mkFileStore "data/test.db"

