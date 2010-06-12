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

\todo[inline]{explain why/what}

In this section, we introduce a block-based heap data structure that is used to
allocate and use blocks of binary data on disk. The structure of the heap is
similar to that of in-memory heaps as used by most programming languages to
manage dynamically allocated data.

The heap uses a file to store a contiguous list of blocks of binary data. Each
of the blocks contains a header and a payload. The header contains flag to
tell us if the block is currently free or in use. Furthermore, the header
indicates the size of the block.
The payload is an arbitrary sequence of binary data. The size of the payload
must not exceed the size specified in the header minus the header size.
An example layout of the heap is shown in Figure~\ref{fig:heap}.

\begin{figure}[tp]
\begin{center}
\includegraphics[scale=0.25]{img/heap.pdf}
\end{center}
\caption{A snippet of a heap structure containing four blocks of which two
block are in use and contain a payload. The blocks are placed next to each
other. An in-memory allocation map is used to map payload sizes to free block
blocks of data.}
\label{fig:heap}
\end{figure}

Applications that use the heap can allocate blocks of data of any size and use
it to freely write to it and read back the payload. All access to the heap is
managed using pointers. The pointer datatype just stores an integer value that
represents an offset into the heap file. We assume that pointers always
point to the beginning of a block.

> type Offset  = Integer
>
> newtype Ptr (f :: * -> *) a = P Offset
>   deriving Binary

The |Ptr| type uses a phantom type to represent the type stored in the payload
of the block. This way, we can ensure that when we read from and write to
a block, it always happens with values of the same type.
\todo{explain why it fits better to use both |f| and |a|}

The |allocate| function can be used to allocate a new block of data that is
large enough to hold payload of the given size. The |allocate| function can be
compared to the in-memory |malloc| function from the C language. The function
returns a pointer to a suitable block on disk.
The |allocate| functions runs in the |Heap| context that we explain below.

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
offsets. The offsets represent blocks that have a payload of exactly the size
they are related to. By maintaining an in-memory allocation map the allocation
operations can efficiently find free blocks of data, without any disk-access.
When a program requests a block of data with a payload that is bigger than can
be found in the allocation map, a new block is allocated at the end of the
heap file.

When a block is no longer needed, it can be freed using the |free| operation:

> free :: Ptr f a -> Heap ()

The |Heap| context in which the heap operations run is a simple monad
transformer stack that uses the |IO| monad on the inside. The context uses a
reader monad to distribute the file handle of the heap file to all operations,
and it makes use of a state monad to manage the allocation map:

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

The |update| heap operation takes a heap pointer and a Haskell
value of type |f a|. It writes a binary serialization of the value to the
payload of the block:

> update :: Binary (f a) => Ptr f a -> f a -> Heap ()

To produce a binary serialization of a Haskell value, the |Binary|
type class is used~\cite{databinary}. The class interface is shown in
Figure~\ref{fig:binaryclass}.
The |put| method serializes a value to a binary stream
value to a binary stream, whereas |get| deserializes a binary stream back to a
Haskell value.\footnote{Both |Get| and |Put| are monads defined in the |Binary|
class. They are not relevant for our purposes here.
Using the @regular@~\cite{jpm} library for generic
programming with regular datatypes, we have created a generic function that can
be used to automatically derive |Binary| instances for a large class of Haskell
datatypes.} The |update| function itself is \emph{unsafe}: if the binary
serialization of the value is larger than the block payload, the function
cannot store the entire value. We solve the problem by wrapping the unsafe
|update| operation in a safe |write| function that takes a Haskell value,
serializes it to a binary stream, allocates just the right amount of data
on the heap and then safely stores the value:

> write :: Binary (f a) => f a -> Heap (Ptr f a)

The last two heap operations are for reading values from an existing block. The
|read| and |fetch| function both take a heap pointer, read the binary payload,
deserialize the payload to a Haskell value using the |Binary| type class and
return the value:

> read   ::  Binary (f a) => Ptr f a -> Heap (f a)
> fetch  ::  Binary (f a) => Ptr f a -> Heap (f a)

The |read| function leaves the original block intact, whereas the |fetch|
frees the block after reading the data.

The |allocate|, |free| and |update| heap operations are used as building
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

