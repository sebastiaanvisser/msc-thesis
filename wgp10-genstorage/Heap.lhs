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
> import qualified Data.Map as M
> import System.IO

%endif

\section{File based storage heap}
\label{sec:heap}

\todo{explain why/what}

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
\begin{center}
\includegraphics[scale=0.25]{img/heap.pdf}
\end{center}
\caption{A snippet of a heap structure containing four blocks of which two
block are in use and contain a payload. The blocks are placed next to each
other. An in-memory allocation map is used to map payload sizes to free block
blocks of data.}
\label{fig:binarytree}
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
|Heap| context that we explain later.

> type Size  = Integer
>
> allocate :: Size -> Heap (Ptr f a)

%if False

> allocate size =
>   do  AllocMap end notused <- get
>       case atLeast size notused of
>         offset:_  -> useBlockAt offset
>         _         -> useBlockAt end
>   where atLeast s =
>           M.elems . M.filterWithKey (\k _ -> k >= s)

> useBlockAt :: a
> useBlockAt  = undefined

%endif
                       

The heap structure uses an in-memory \emph{allocation map} to track the current
heap size and all blocks in the heap that are not currently used:

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

%if False

>   deriving
>     ( Functor, Monad
>     , MonadIO
>     , MonadReader Handle
>     , MonadState  AllocMap
>     )

%endif

The |update| heap operations takes a pointer to a heap block and a Haskell
value of type |f a| and writes a binary serialization of the value to the
payload of the block:

> update :: Binary (f a) => Ptr f a -> f a -> Heap ()

To produce a binary serialization of a Haskell value the |Binary|
\cite{databinary} type class is used. The |Binary| class has to methods, as
illustrated in figure \ref{fig:binaryclass}, one method |put| for serializing a
value to a binary stream and |get| for deserializing a binary stream back to a
Haskell value.\footnote{Using the |Regular| \cite{jpm} library for generic
programming with regular datatypes we have created a generic function that can
be used to automatically derive |Binary| instances for a large class of Haskell
datatypes.} The |update| function itself is \emph{unsafe}, when the binary
serialization of the value is larger than the block payload, the function
cannot store the entire value. We solve wrap the unsafe |update| function in a
safe |write| function that takes a Haskell value, serializes this to a binary
representation, allocates just the right amount of data on the heap and can
then safely store the value:

> write :: Binary (f a) => f a -> Heap (Ptr f a)

The last two heap operations are for reading values from an existing block. The
|read| and |fetch| function both take a pointer to block, read the binary
serialization from the payload, deserialize the bytes to a Haskell value using
the |Binary| type class and return the value:

> read   :: Binary (f a) => Ptr f a -> Heap (f a)
> fetch  :: Binary (f a) => Ptr f a -> Heap (f a)

The |read| function leaves the original block intact, the |fetch| functions
frees the block after reading the data.

Both the |allocate|, |free| and |update| heap operations are used as building
blocks for the |write|, |read| and |fetch| operations, but are not used further
in this framework. In the next section we see how the latter three function are
used in combination with the annotation framework to build persistent data
structures.

Because the |Heap| context is a monad we can easily combine different operation
into one using the monadic bind operator, or Haskell's do-notation. We allow
running a heap operation against a file on disk using the |run| function.

> run :: FilePath -> Heap a -> IO a

Because of the file access the |run| function works in the |IO| monad. The
|run| function opens the heap file and initializes it when it is new. When the
file does exist it quickly scans all blocks to compute the in-memory allocation
map. After this it applies the heap computation and closes the file.

\todo{explain what we have done and why}

%if False

> read = undefined
> fetch = undefined
> write = undefined
> update = undefined
> free = undefined
> run = undefined

%endif

\begin{figure}[t]
\begin{center}
\begin{spec}

class Binary t where
  put  :: t -> Put
  get  :: Get t

\end{spec}
\end{center}
\caption{The |Binary| type class}
\label{fig:binaryclass}
\end{figure}

