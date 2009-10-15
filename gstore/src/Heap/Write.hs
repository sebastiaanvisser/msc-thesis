module Heap.Write where

import Control.Applicative
import Control.Monad.Lazy
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary
import Data.Maybe
import Data.Record.Label
import Data.Word
import Prelude hiding (read)
import System.IO.Binary
import Heap.Alloc hiding (Heap)
import Heap.Block
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Heap.Alloc as A
import qualified Heap.Read as R

newtype Heap a = Heap { run :: A.Heap a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState FileHeap
    , MonadReader Handle
    )

instance LiftLazy R.Heap Heap where
  liftLazy = Heap . liftLazy

store :: Binary (f a) => f a -> Heap (Pointer f a)
store f =
  do let bs = encode f
     block <- (Heap . A.allocate . fromIntegral . B.length) bs
     write bs block
     return (Ptr (_offset block))

writeHeader :: Offset -> Header -> Heap ()
writeHeader o (x, s) =
  do h <- ask
     liftIO $
       do hSeek h AbsoluteSeek (fromIntegral o)
          write8  h (if x then (0x23::Word8) else 0x00)
          write32 h s

writeBlock :: Block -> Heap ()
writeBlock (Block o s d) =
  do writeHeader o (isJust d, s)
     h <- ask
     when (isJust d) $
       liftIO (B.hPut h (B.take (fromIntegral s) (fromJust d)))

write :: B.ByteString -> Block -> Heap ()
write bs = writeBlock . set payload (Just bs)

