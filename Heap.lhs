%include polycode.fmt
%include thesis.fmt

%if False

> {-# LANGUAGE KindSignatures, GeneralizedNewtypeDeriving #-}
> module Heap where

> import Data.Binary
> import Data.IntMap hiding (update)
> import Control.Monad.Reader
> import Control.Applicative hiding (liftA)
> import Control.Monad.State hiding (get, put)
> import System.IO
> import Prelude hiding (read)
> import qualified Data.ByteString.Lazy as Bs
> import Data.ByteString.Lazy (ByteString)

%endif

\chapter{Persistent Heap}
\label{chap:heap}

The previous chapter explained how to use generic programming with annotated
fixed points to associate custom functionality to traversals over recursive
datatypes. The goal of this project is to use this annotation framework to
project operations on recursive datatypes to a persistent instance on the
computer's hard disk. To make this goal possible we are in need for a flexible
storage structure that allows us to freely read and write data to disk.

In this chapter we introduce a storage heap that uses generic binary
serialization for values of arbitrary Haskell datatypes on disk. The structure
of this heap is very similar to the structure of in-memory heaps used by most
programming language implementations to store pointer data. The heap allows the
basic operations of allocating new data blocks, freeing existing data blocks,
reading from and writing to data blocks. The current implementation of the heap
structure that ships with this project is not fully optimized for speed and is
just a prototype implementation. One single heap structure is stored in one
single file that can shrink and grow in size on demand.

All operations on the heap work in a monadic context that are built on top of
the |IO| monad.  To make it easier to reason about the behaviour of the storage
heap we introduce a stacked heap monad. We define one context for
\emph{read-only} access to the underlying data, one context for
\emph{read-write} access to the data, and one context for \emph{allocating and
freeing} new blocks of data. We call the read-only context |HeapR|, the
read-write context |HeapW| and the allocation context |HeapA|. The top context
is the |HeapW|, which internally uses the |HeapA| context, which on its turn
uses the |HeapR| on the inside. This separation of context is introduced to be
able to differentiate between the possible types of actions.  As we will see in
the next chapter \todo{xxx} this separation is essential for gaining control
over the laziness of our storage framework.

In this chapter we introduce seven basic heap operations, some of which we
will be using in the next chapters, some of which will only be used internally.

\begin{spec}
allocate  :: Integer                              -> HeapA  (Pointer a)
free      :: Pointer a                            -> HeapA  ()
update    :: Binary a => Pointer a -> ByteString  -> HeapW ()

read      :: Binary a => Pointer a                -> HeapR  a
fetch     :: Binary a => Pointer a                -> HeapW  a
write     :: Binary a => a                        -> HeapW  (Pointer a)
run       :: FilePath                             -> HeapW  a -> IO a
\end{spec}

The first three operations are potentially unsafe and will only be used as
building blocks for the four last functions. We will explain these heap
operations after showing the global layout of the heap file.

\section{Heap layout}
\label{sec:heaplayout}

On disk the heap consists of one linear contiguous list of blocks. A block
is a single atomic storage unit that can contains an arbitrarily large string
of bytes. Every block consist out of a single byte indicating whether the block
is occupied with data or free for allocation, a four byte size indication
representing the byte size of the payload and the payload itself which is
a stream of bytes. The size of the payload is always equal to the byte size
stored in the block header.

\begin{figure}[hp]
\begin{center}
\includegraphics[angle=90,scale=0.6]{./heap.pdf}
\end{center}
\caption{Storage heap with 2 occupied and 2 free blocks. The top bar represents
the actual heap on disk, the bottom bar represents the in-memory allocation map
pointing to the offsets of the free blocks.}
\end{figure}

The Haskell code uses a |Pointer| datatype to refer to these storage blocks.
Pointers are just integer values pointing to the exact byte offset of the block
in the file that stores the heap. The |Pointer| datatype is indexed with a
phantom type indicating the original type of the data stored in the block.
Because |Pointer|s are stored on disk we need a |Binary| instance for it, which
can be derived automatically because the type is just a |newtype| over an
|Integer|.

> newtype Pointer a = Ptr { unPtr :: Integer }                               
>   deriving Binary

Before we continue with the description of the heap operations we quickly
explain the |Binary| type class. The class contains two methods, |get| and
|put|.

\begin{spec}
class Binary t where
  get  :: Get t
  put  :: t -> Put
\end{spec}

The |get| and |put| methods can be used to build computations for deserializing
and serializing values from and to |ByteString|s. Take this example |Binary|
instance for the |Either a b| type, defined in the \texttt{Data.Binary}
package.

\begin{spec}
instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left  a) = do putWord8 0 ; put a
    put (Right b) = do putWord8 1 ; put b
    get = do  w <- getWord8
              case w of
                  0 -> liftM Left  get
                  _ -> liftM Right get
\end{spec}

When an instance for |Binary| is defined for a certain type we can use the
|decode| and |encode| functions from the library to convert strings of bytes to
Haskell values and vice versa.

\begin{spec}
encode  :: Binary a =>  a -> ByteString
decode  :: Binary a =>  ByteString -> a
\end{spec}

The following example shows how to use the |encode| and |decode| functions.

\begin{verbatim}
ghci> encode (Right 'a' :: Either Bool Char )
Chunk "\SOHa" Empty
ghci> decode it :: Either Bool Char
Right 'a'
\end{verbatim}

From the example |Binary| instance for the |Either a b| type we can see that
|Binary| instances are built very systematically. Using the generic programming
library |Regular| we have created two generic functions to automatically derive
the |get| and |put| methods for arbitrary Haskell types.\footnote{The code for
this package can be found at:\\
\url{http://hackage.haskell.org/package/regular-extras}}

\section{Reading}
\label{sec:heapread}

We can read from a storage block using the |read| operation. The read operation
takes a pointer to a storage block and tries to return a value of the Hakell
datatype indicated by the phantom type in the input pointer. When the
operation fails, possibly due to an invalid pointer or some I/O error, the
function throws an exception. When the functions succeeds to read the stream
of bytes from disk it uses |get| function from the |Binary| type class to
deserialize the data to a true Haskell value.

The |read| functions runs inside the read-only heap context |HeapR|. This
context is defined as a |ReaderT| monad transformer wrapping |IO|. The value
stored inside the reader monad is the file |Handle| of the heap file.

> newtype HeapR a = HeapR (ReaderT Handle IO a)
>   deriving (Functor, Applicative, Monad, MonadIO)

%if False

> read :: Binary a => Pointer a -> HeapR a
> read = undefined

%endif

We now give the implementation of the |read| function in pseudo code.

\begin{spec}
read :: Binary a => Pointer a -> HeapR a
read ptr =
  do  file <- ask                      -- Get the heap file handle.
      liftIO (seek file (offset ptr))  -- Move file cursor to pointer offset.
      s   <- readBlockSize             -- Read the block size from the header.
      bs  <- readPayload s             -- Read the ByteString from the current payload.
      return (decode bs)               -- Decode the bytes to a Haskell value.
\end{spec}

The actual code involves additional sanity checks that are left out in this
example. The |read| function simply moves the current file position to the
start of the block and reads the size of the block from the header. The header
size is used to read the payload into a |ByteString|. This |ByteString| is
decoded and returned as a true Haskell value.

\section{Allocating and freeing}
\label{sec:heapalloc}

When an application requires to store a new value on the storage heap, we need
to allocate a new data block with the right size. The |allocate| functions
allocates new blocks by manipulating an in-memory |AllocationMap|. This
datatype represents a mapping from block offsets to a list of payload sizes,
for all blocks on the storage heap that are currently unoccupied. Additionally,
the map stores the total size of the heap.

> data AllocationMap = AllocationMap
>   {  map   :: IntMap [Int]
>   ,  size  :: Int
>   }

The |allocate| function takes an integer value indicating the size the new
block should at least have.  The function returns a |Pointer| to an exact block
offset. The block pointed to by this offset can now we be used to store
information. The phantom type of the |Pointer| is still polymorphic because the
allocation function does not require this information.

> allocate :: Integer -> HeapA (Pointer a)

The |allocate| function performs a lookup in the |AllocationMap| for a block of
sufficient size. When there is no large enough free block available the
|allocate| function can join up two or more consecutive free block and use this
joined result block. When joining two blocks is not possible the |allocate|
function creates a new block with sufficient size at the end of the heap,
thereby growing the heap.  Because all the administration is in-memory, the
allocation itself does not require any disk access.
  
%if False

> allocate = undefined

%endif

As the type signature of |allocate| indicates there is a separate |HeapA|
context for the allocation operations.  The |HeapA| context uses a |StateT|
monad transformer to supply all operations with the current |AllocationMap|.
This contexts internally wraps the |HeapR| context.

> newtype HeapA a = HeapA (StateT AllocationMap HeapR a)
>   deriving (Functor, Applicative, Monad, MonadIO)

Besides allocation of new blocks, the |HeapA| context also supports the
possibility to free existing blocks. 

> free :: Pointer a -> HeapA ()

%if False

> free = undefined

%endif

This operation add the block pointed to by the input |Pointer| to the
allocation map. The block can now be reused when needed. This operation also
runs in-memory and does not require any disk access.

\section{Writing}
\label{sec:heapwrite}

The third heap context is the writing context |HeapW| that allows write access
to the heap structure. The |HeapW| context directly wraps the |HeapA| context,
thereby allowing computations to write, allocate, free and read.

> newtype HeapW a = HeapW (HeapA a)
>   deriving (Functor, Applicative, Monad, MonadIO)

To be able to also perform read and allocation operations in the |HeapW|
context, we define two lift functions |liftR| and |liftA| that lift operation
into the read-write context. 

> liftR :: HeapR a -> HeapW a
> liftA :: HeapA a -> HeapW a

%if False

> liftA = undefined
> liftR = undefined

%endif

The first function that runs inside the |HeapW| context is the |update| function.
This function takes a value of some Haskell datatype and a pointer to a block
and updates the payload of that block with the binary serialization of the
value. The function uses the |put| function from the |Binary| type class to
perform the serialization.

We show the implementation of the update function in pseudo code.

%if False

> update :: Binary a => Pointer a -> ByteString -> HeapW ()
> update = undefined

%endif

\begin{spec}

update :: Binary a => Pointer a -> ByteString -> HeapW ()
update ptr bs =
  do  file <- ask
      liftIO (seek file (offset ptr))   -- Move file cursor to pointer offset.
      s   <- readBlockSize              -- Read block size from the header.
      when (s < length bs)              -- When payload to small...
        (throw SomeError)               -- ...throw some error.
      writePayload bs                   -- Write bytes to the payload.

\end{spec}

This update function is very useful but unsafe, when the payload of the input
block is not large enough to hold the string of bytes this function cannot
write the value to disk. When there is not enough data available in the block
the function throws an exception.

Because the |update| function is unsafe we implement a safe function |write| on
top of it. The |write| operation only takes a value as input and itself
allocates a storage block just big enough to hold the serialized value.  The
payload of the allocated block is updated with the serialized value, which now
always succeeds. The function returns a pointer to the allocated block.

> write :: Binary a => a -> HeapW (Pointer a)
> write a =
>   do  let bs = encode a
>       block <- liftA (allocate (fromIntegral (Bs.length bs)))
>       update block bs
>       return block

It can be useful to read a block from the heap and then immediately free it,
because it is guaranteed not to be read again. For this purpose, we define a
function |fetch| that is a combination of a |read| and a |free|. 

> fetch :: Binary a => Pointer a -> HeapW a
> fetch p = do  r <- liftR (read p)
>               liftA (free p)
>               return r

We lift this function to the |HeapW| context so we can easily run it side by
side with the write function, without first lifting it.

\section{Root node}
\label{sec:rootnode}

The storage heap layout does not force any structure on the data one might
save. The only requirement is that the storage is block based, but no relations
between separate blocks is required. As we will see in chapter
\ref{chap:storage}, we will eventually reuse the structure of recursive
datatypes to guide the way we make relations between individual blocks.

Every data structure has at least one root node and for every operation we need
this root node. To make the root node accessible, we make the first storage
block special and dedicate it to store the root of our data structure. We call
this block, at offset 0, the \emph{null block}. We define three helper
functions for working with the assumption the null block stores a pointer to
the root of the data structure root.

The |query| function asks a read-only heap action that takes as input the root
of the data structure. Altough the action will be lifted to the root heap
context, the |HeapW|, the action itself is cannot perform write actions.

> query :: (Pointer f -> HeapR c) -> HeapW c
> query c = liftR (read (Ptr 0) >>= c)

The |produce| function is used to create new structures from scratch. After the
producer function has built up the data structure, a pointer to the structure
root will be stored in the null block.

> produce :: HeapW (Pointer f) -> HeapW ()
> produce c = c >>= update (Ptr 0)

The |modify| function takes a computation that transforms an existing structure
into a new structure. The original structure is read from the null block, a
pointer to the newly created structure will be stored at the null block again.

> modify :: (Pointer f -> HeapW (Pointer f)) -> HeapW ()
> modify c = liftR (read (Ptr 0)) >>= c >>= update (Ptr 0)

These functions are rather useless when manually storing blocks of data in the
storage heap, but become very useful when the heap is used to store true data
structures. We will see this in action in section \ref{sec:persistenttree}.

\section{Running the heap operations}
\label{sec:runheap}

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
topmost context. Operations in the |HeapA| and |HeapR| contexts must be lifted
before they can be performed. The |run| function start by opening the file and
storing the file handle in the state monad defined by the |HeapR| context. It
then traverses all blocks on the heap to generate the in-memory allocation map
with all the free blocks. The resulting map is stored inside the |HeapA|
context. After the allocation map is read the |run| function applies the
input operation to the heap structure. When the computation has finished the
blocks in the heap on file are synchronized with the possibly updated
allocation map.

The following example shows how we can open up an existing heap, store two
strings, read one string back in and print the result. The block is freed
immediately after using it. When the computation is finished the heap on file
contains one additional block containing the second string. When the file does
not exists an new empty heap is created.

> main :: IO ()
> main = run "test.heap" $
>   do  p0   <- write "First block"
>       _    <- write "Second block"
>       str  <- liftR (read p0)
>       liftIO (putStrLn str)
>       liftA (free p0)
>       return ()

%if False
$
%endif

Because the entire heap is stored in the \texttt{test.heap} file, we can use
invoke this mini-program several times consecutively. Each time the function
runs, a new block is left on the heap containing the value \texttt{"Second
Block"}.

In this section we have described a very basic file based heap structure with a
simple interface that allows us to allocate and write new blocks of
information, to read from existing blocks of information and to free blocks
that are no longer of any use. The heap manages allocation and reuse of blocks
of data, but does not know of any structure of the information. Any relation
between blocks of data is up to the user of the heap. The next chapter shows
how we can connect our generic annotation framework for recursive datatypes to
the storage heap to obtain a generic storage framework.

