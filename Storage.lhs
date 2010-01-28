%include polycode.fmt
%include thesis.fmt

%if False

> module Storage where

%endif

\section{Persistent heap}

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
storage heap we introduce two different monadic contexts, one for
emph{read-only} access to the underlying data and one for \emph{read-write}
access to the data. The call the read-only context |HeapR| and the read-write
context |HeapW|, the |HeapW| context internally uses the |HeapR| context.

The next chapter on the storage annotation will show why it is useful to
differentiate between read-only and read-write parts of the heap. We now first
explain the internal layout and basic operations.

\subsection{Heap layout}

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

Although not strictly necessary in the general case, but quite useful in our
use case, the phantom type always indicates a container type with an associated
value type. Because |Pointer|s will most likely be stored on disk we need a
|Binary| instances for it, which can be derived because the type is just a
|newtype| over an |Integer|.

> newtype Pointer (f :: * -> *) a = Ptr { unPtr :: Offset }                               
>   deriving Binary

\subsection{Allocation}

In memory map contains free blocks.
Allocation does not require disk access.
at load time allocation map is read.

\subsection{Reading}

reading a block asks for a pointer and gives back the contents

\subsection{Writing}

writing takes a value a allocates and write.

\subsection{Running}

run function






\section{Generic storage}
\subsection{Storage annotation}
\subsection{Persistent map}

