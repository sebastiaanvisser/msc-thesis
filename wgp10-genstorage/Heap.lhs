%if False

> {-# LANGUAGE
>     ScopedTypeVariables
>   , GeneralizedNewtypeDeriving
>   , KindSignatures
>   #-}
> module Heap where

> import Control.Monad.Reader
> import Control.Monad.State
> import Data.Binary (Binary)
> import Data.Map (Map)
> import Prelude hiding (lookup, read)
> import System.IO

%endif

\section{File based storage heap}

In this section we introduce a block based heap data structure that is used to
allocate and use blocks of binary data on disk. The structure of the heap is
similar of in-memory heaps used by most programming languages to manage
dynamically allocated data.

The heap uses a file to store a contiguous list of blocks of binary data. Each
of the blocks contains a header and a payload. The header contains flag to
tells us if the block is free or currently in-use and indication of the size of
the block. The payload is an arbitrary sequence of binary data, at most the
size stored in the payload minus the header size. 

\begin{figure}[h]
\label{fig:binarytree}
\begin{center}
\includegraphics[scale=0.25]{img/heap.pdf}
\end{center}
\caption{A snippet of a heap structure containing for blocks of which two block
are in use and contain a payload. The blocks are placed next to each other. An
in-memory allocation map is used to map payload sizes to free block blocks of
data.}
\end{figure}

Applications that use the heap can allocate blocks of data of any size and use
it to freely write to it and read back the payload. All access to the heap is
managed using pointers. The pointer datatype just stores an integer value that
represents an offset into the heap file. We assume pointers always point to the
beginning of a block.

> type Offset  = Integer
>
> newtype Ptr (f :: * -> *) a = P Offset
>   deriving Binary

The |Ptr| type uses a phantom type to represent the type stored in the payload
of the block. This way we can ensure only that we can only read information
from a block with the same type as we have originally written it. 
\todo{explain why it fits better to use both |f| and |a|}

The |allocate| function can be used to allocate a new block of data with a
payload size at least the requested size. The |allocate| function can be
compared to the in-memory |malloc| function from the C language. The function
returns a pointer to the block on disk. The |allocate| functions runs in the
|Heap| context that we will explain later.

> type Size  = Integer
>
> allocate :: Size -> Heap (Ptr f a)

The heap structure uses an in-memory \emph{allocation map} to track the current
heap size and all blocks in the heap that are not currently used, and free to
be used for the allocation of new data:

> data AllocMap = AllocMap
>   {  heapsize  :: Size
>   ,  unused    :: Map Size [Offset]
>   }

The allocation map is a finite mapping from size to a list of
offsets. The offsets represents blocks that have a payload of exactly the size
they are related to. By maintaining an in-memory allocation map the allocation
operations can efficiently find free blocks of data, without any disk-access.
When a program request a block of data with a payload that is bigger than can
be found in the allocation map, a new block is allocated at the end of the
heap file.

Freeing a block of data when no longer used is done using the |free| operation:

> free :: Ptr f a -> Heap ()

The |Heap| context in which the heap operations run is a simple monad
transformer stack that uses the |IO| monad on the inside. The context uses a
reader monad to provide operations with the file handle of the heap file and
use a state monad to provide the allocation map:

> newtype Heap a = Heap
>   (ReaderT  Handle (StateT AllocMap IO ) a)

where:

%if False

>   deriving
>     ( Functor, Monad
>     , MonadIO
>     , MonadReader Handle
>     , MonadState  AllocMap
>     )

%endif

> read      :: Binary (f a)  =>  Ptr f a                 -> Heap (f a)
> fetch     :: Binary (f a)  =>  Ptr f a                 -> Heap (f a)
> update    :: Binary (f a)  =>  Ptr f a -> f a          -> Heap ()
> write     :: Binary (f a)  =>  f a                     -> Heap (Ptr f a)
> run       :: FilePath  ->  Heap a                      -> IO ()

%if False

> read = undefined
> fetch = undefined
> write = undefined
> update = undefined
> allocate = undefined
> free = undefined
> run = undefined

%endif

\begin{figure}[t]
\label{fig:binaryclass}
\begin{center}
\begin{spec}

class Binary t where
  put :: t -> Put
  get :: Get t

\end{spec}
\end{center}
\caption{The |Binary| type class}
\end{figure}

