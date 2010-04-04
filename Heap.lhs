%include polycode.fmt
%include thesis.fmt

%if False

> {-# LANGUAGE KindSignatures, GeneralizedNewtypeDeriving #-}
> module Heap where

> import Data.Binary
> import Data.IntMap
> import Control.Monad.Reader
> import Control.Applicative hiding (liftA)
> import Control.Monad.State
> import System.IO
> import Prelude hiding (read)

%endif

\chapter{Persistent heap}

The previous chapter explained how to use generic programming with annotated
fixed points to associate custom functionality to traversals over recursive
datatypes. The goal of this project is to use this generic annotation framework
to project operations on recursive datatypes to a persistent instance on the
computer's hard disk. To make this possible we are in need for a flexible
storage structure that allows us to freely read and write data to disk.

This chapter will introduce a storage heap that uses generic binary
serialization to values of arbitrary Haskell datatypes on disk. The structure
of this heap will be very similar to the structure of in-memory heaps used by
most programming language implementations to store pointer data. The heap will
allow the basic operations of allocating new data blocks, freeing existing data
blocks, reading from and writing to data blocks. The current implementation of
the heap structure that ships with this project is not fully optimized for
speed and is just a prototype implementation. One single heap structure is
stored in one single file that can shrink and grow in size on demand.

All operations on the heap with work in some monadic context that uses the |IO|
monad on the inside. To make it easier to reason about the behaviour of the
storage heap we introduce two\footnote{As we will see later on in this chapter
there are more than two heap contexts available, but only two will be publicly
usable.} different monadic contexts, one for emph{read-only} access to the
underlying data and one for \emph{read-write} access to the data. The call the
read-only context |HeapR| and the read-write context |HeapW|, the |HeapW|
context internally uses the |HeapR| context.

The next chapter on the storage annotation will show why it is useful to
differentiate between read-only and read-write parts of the heap. We now first
explain the internal layout and basic operations.

\section{Heap layout}

On disk the heap consist out of one linear contiguous list of blocks. A block
is a single atomic storage unit that can contains an arbitrarily large string
of bytes. Every block consist out of a single byte indicating whether the block
is occupied with data or free for allocation, a four byte size indication
representing the byte size of the payload and the payload itself which is
n-byte stream of data. 

\begin{figure}[h]
\includegraphics[scale=0.25]{./img/heap+map.png}
\caption{Storage heap with 2 occupied and 2 free blocks.}
\end{figure}

The Haskell code uses a |Pointer| datatype to refer to these storage blocks.
Pointers are just integer values pointing to the exact byte offset of the block
in the file that stores the heap. The |Pointer| datatype is indexed with a
phantom type indicating the original type of the data stored in the block.
Because |Pointer|s will most likely be stored on disk we need a |Binary|
instance for it, which can be derived because the type is just a |newtype| over
an |Integer|.

> newtype Pointer a = Ptr { unPtr :: Integer }                               
>   deriving Binary

\section{Reading}

Reading from a storage block can be done with the |read| operation. The read
operation takes a pointer to a storage block and tries to return a value of the
Hakell datatype indicated by the phantom type in the input pointer. When the
operations fails, possibly due to an invalid pointer or some I/O error, the
functions produces an exception. When the functions succeeds to read the stream
of bytes from disk it uses the |Binary| type class to deserialize the data to a
true Haskell value.

> read :: Binary a => Pointer a -> HeapR a

%if False

> read = undefined

%endif

The read-only |HeapR| context only allows read-only access to the underlying
heap structure and is defined as a |ReaderT Handle| monad transformer over the
|IO| monad. The reader monad makes sure all operations in this context can
access the file handle of the heap.

> newtype HeapR a = HeapR (ReaderT Handle IO a)
>   deriving (Functor, Applicative, Monad, MonadIO)

\section{Allocation and Freeing}

Allocating new blocks is done in memory by manipulation of an allocation map.
The allocation map stores mapping from blocks offsets to payload sizes for
all blocks on the storage heap that are free. The |allocate| function performs
a simple lookup in this table for a block of sufficient size. When there is no
large enough free block available the |allocate| function might join up two or
more consecutive free block and use this joined result block. When not even
this process can be performed the function will create a new block with
sufficient size at the end of the heap, this way the heap size grows. Because
all the administration is in-memory, the allocation itself does not require any
disk access.
  
The |allocate| function returns a |Pointer| to the exact block offset. The
phantom type of the |Pointer| is still polymorphic because the allocation
function does not require this information.

> allocate :: Integer -> HeapA (Pointer a)

%if False

> allocate = undefined

> data AllocationMap = 
>   AllocationMap
>     { map  :: IntMap [Int]
>     , size :: Int
>     }

%endif

As the type signature of |alloc| indicates there is a separate |HeapA| context
for the allocation operations. This context is not likely to be used as the
public interface, but can be used as a basis for building the write operations.
The |HeapA| context uses a |StateT| monad transformer to supply all
operations with the current |AllocationMap|.

> newtype HeapA a = HeapA (StateT AllocationMap HeapR a)
>   deriving (Functor, Applicative, Monad, MonadIO)

Besides allocation of new blocks of data the |HeapA| context also supports the
possibility to free existing blocks. This operations can also run in memory by
just adding a newly freed block to the allocation map.

> free :: Pointer a -> HeapA ()

%if False

> free = undefined

%endif

\section{Writing}

The third, and top most, heap context is the writing context |HeapW| that
allows write access to the heap structure. The |HeapW| context wraps the
|HeapA| context also allowing allocation and read operation to be run inside
this context.

> newtype HeapW a = HeapW (HeapA a)
>   deriving (Functor, Applicative, Monad, MonadIO)

The first function that runs inside the |HeapW| context is the update function
that takes a value of some datatype for which there is a binary instance
available and pointer to a block and updates the payload of that block.

> update :: Binary a => a -> Pointer a -> HeapW ()

%if False

> update = undefined

%endif

This update function is very useful but unsafe, when the payload of the input
block is not large enough to hold the binary serialization of the input value
this function cannot write the value to disk.

Because the |update| function is unsafe we consider it a private function and
implement a safe public function |write| on top of it. The |write| operation
only takes a value as input and will itself allocate a storage block just big
enough to hold the serialized value. The payload of the allocated block will be
updated with the serialized value, which will no always succeed. The function
will return a pointer to the allocated block.

> write :: Binary a => a -> HeapW (Pointer a)
 
%if False

> write = undefined

%endif

\section{Running}

Now that we have defined three different heap contexts together with some basic
operations for reading, writing, allocating and freeing blocks of data we can
define a |run| function that can apply these operations to a file.

The |run| function takes a path to the file containing the heap structure and a
computation in the read-write context and performs the operations on the file.

> run :: FilePath -> HeapW a -> IO a

%if False

> run = undefined

%endif

The |run| function only works for the |HeapW| context because this is the
topmost context itself containing all the lower contexts. The |run| function
start by opening the file and storing the file handle in the state monad
defined by the |HeapR| context. It then traverses all blocks on the heap to
generate the in-memory allocation map with all the free blocks, this map is
stored inside the |HeapA| context. After the allocation map is read the |run|
function can apply the input operation to the heap structure.When the
computation has finished the blocks in the heap on file will be synchronized
with the possibly updated allocation map.

To be able to also perform read and allocation operations in the run
environment we define two lift functions |liftR| and |liftA| that lift
operation into the read-write context. 

> liftR :: HeapR a -> HeapW a
> liftA :: HeapA a -> HeapW a

%if False

> liftA = undefined
> liftR = undefined

%endif

The following example shows how we can open up an existing heap -- when the
file does not exists an new empty heap will be created -- store two strings,
read one string back in and print the result. The block will be freed
immediately after using it. When the computation is finished the heap on file
will contain one additional block containing the second string. 

> main :: IO ()
> main = run "test.heap" $
>   do  p0   <- write "First block"
>       _    <- write "Second block"
>       str  <- liftR (read p0)
>       liftIO (putStrLn str)
>       liftA (free p0)
>       return ()

In this section we have described a very basic file based heap structure with a
simple interface that allows us to allocate and write new blocks of
information, to read from existing blocks of information and to free blocks
that are no longer of any use. The heap manages allocation and reuse of blocks
of data, but does not know of any structure of the information. Any relation
between blocks of data is up to the user of the heap. The next chapter shows
how we can connect our generic annotation framework for recursive datatypes to
this storage heap to gain a generic storage framework.

