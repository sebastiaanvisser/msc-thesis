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

\section{File-based storage heap}
\label{sec:heap}

In the previous sections, we showed how to perform generic programming with
fixed point annotations. The annotations form the basis of our storage
framework. We use the annotations to marshal individual nodes from and to a
database file on disk. Before we can explain this storage annotation in more
detail, we first sketch the low-level storage layer.

In this section, we introduce a block-based heap data structure that is used to
allocate and use fragments of binary data on disk. The structure of the heap is
similar to that of in-memory heaps as used by most programming languages to
manage dynamically allocated data.
The heap uses a file to store a contiguous list of blocks of binary data. Each
of the blocks contains a header and a payload. The header contains a flag to
tell if the block is currently free or in use. Furthermore, the header
indicates the size of the block.
The payload is an arbitrary sequence of binary data. The size of the payload
must not exceed the size specified in the header minus the header size.
An example layout of the heap is shown in Figure~\ref{fig:heap}.

The heap described here has a rather imperative and low level implementation.
Some of the operations are unsafe, because they have invariants not checked at
compile time. We discourage direct use of such unsafe structure and will show
how to wrap the low-level operations into a safer and high-level domain in the
next section.

\begin{figure}[tp]
\begin{center}
\includegraphics[scale=0.25]{img/heap.pdf}
\end{center}
\caption{A snippet of a heap structure containing four blocks of which two
block are in use and contain a payload. The blocks are placed next to each
other. An in-memory allocation map is used to map payload sizes to free
blocks of data.}
\label{fig:heap}
\end{figure}

\subsection{Offset pointers}

Applications that use the heap can allocate blocks of data of any size and use
it to freely write to it and read back the payload. All access to the heap is
managed using pointers. The pointer datatype just stores an integer value that
represents an offset into the heap file. There is an invariant that pointers always
point to the beginning of a block.

> type Offset  = Integer
>
> newtype Ptr (f :: * -> *) a = P Offset
>   deriving Binary

The |Ptr| type uses a phantom type to represent the type stored in the payload
of the block, this way ensuring we can only read values from a block of with
the same type as has been written to. Because we use the |Ptr| type as a fixed
point annotation, as shown in the next section, we use two phantom type
variables: one of kind |* -> *| and of kind |*|. The |Ptr| type fits the |a|
variable from the annotation type classes. From the perspective of this heap
data structure we could also have used a single phantom type of kind |*|.

\subsection{Heap context}

The implementation of the storage heap consists of a small set of
operations that run inside a monadic |Heap| context. The |Heap| monad
is a monad transformer stack that uses the |IO|
monad on the inside.

> newtype Heap a = Heap (ReaderT Handle (StateT AllocMap IO ) a)

The context uses a reader monad to distribute the file
handle of the heap file to all operations, and it makes use of a state monad to
manage an \emph{allocation map}.

The allocation map stores a mapping of
block sizes to the offsets of all blocks that are not currently in use.
Because we manage an in-memory map no disk access is needed when
allocating new blocks of data.

From the point of view of the user the |Heap| is opaque, no access to the
internals of the monad are required to work with the heap.
In order to run a sequence of heap operations we use the |run| function, that
gets supplied the name of a heap file.

%if False

>   deriving
>     ( Functor, Monad
>     , MonadIO
>     , MonadReader Handle
>     , MonadState  AllocMap
>     )

> type AllocMap = ()

%endif

> run :: FilePath -> Heap a -> IO a

Because of the file access, the result of |run| is in the |IO| monad. The
|run| function opens the heap file and initializes it if it is new. If the
file exists, it quickly scans all blocks to compute the in-memory allocation
map. It then applies the heap computations, and closes the heap file in the end.

\subsection{Heap operations}

In this section we list all the heap operations one by one. We will not give the
implementation of any of the functions, but give their type signature and
describe their usage:

\begin{itemize}

\item | allocate :: Integer -> Heap (Ptr f a) |

The |allocate| operation can be used to allocate a new block of data that is
large enough to hold a payload of the given size. The function can be
compared to the in-memory |malloc| function from the C language. On
return, |allocate| yields a pointer to a suitable block on disk. The function
marks the current block as occupied in the in-memory allocation map.
Consecutive calls the |allocate| will no longer see the block as eligible for
allocation.

\item | free :: Ptr f a -> Heap () |

When a block is no longer needed, it can be freed using the |free| operation.
The internal allocation map will be updated so the block can be reused in later
allocations.

% \item | update :: Binary (f a) => Ptr f a -> f a  -> Heap () |
% 
% The |update| heap operation takes a heap pointer and a Haskell
% value of type |f a|. It writes a binary serialization of the value to the
% payload of the block.

\item | write :: Binary (f a) => f a -> Heap (Ptr f a) |

% The |update| function itself is \emph{unsafe}: if the binary
% serialization of the value is larger than the block payload, the function
% cannot store the entire value. We solve the problem by wrapping the unsafe
% |update| operation in a safe |write| function that takes a Haskell value,
% serializes it to a binary stream, allocates just the right amount of data
% on the heap and then safely stores the value:

The |write| operation takes a Haskell value, serializes it to a binary stream,
allocates just the right amount of data on the heap and then stores the value
in the block on disk.

To produce a binary serialization of a Haskell value, the |Binary|
type class is used~\cite{databinary}. The interface of the class
is as follows:

< class Binary t where
<   put  :: t -> Put
<   get  :: Get t

The |put| method serializes a value to a binary stream,
whereas |get| deserializes a binary stream back to a
Haskell value.\footnote{Both |Get| and |Put| are monads defined in the |Binary|
class. The details are not relevant for our purposes here.
Using the @regular@~\cite{jpm} library for generic
programming, we have created a generic function that can
be used to automatically derive |Binary| instances for Haskell
datatypes that are regular.}

\item | read :: Binary (f a) => Ptr f a -> Heap (f a) |

Dual to the |write| operation we have the |read| operation. 
The function takes a pointer to a block on disk, reads the binary payload,
deserializes the payload to a Haskell value using the |Binary| type class and
returns the value.

\item | fetch :: Binary (f a) => Ptr f a -> Heap (f a) |

The |fetch| operation is a slight variation on the |read| operation. Whereas
|read| leaves the original block intact, |fetch|
frees the block after reading the data.

\item | writeRoot :: Ptr f a -> Heap () |

The |writeRoot| operation is a slight variation of the |write| operation which
will show to be useful in the next section. The function takes a pointer to
some block on the heap and will store \emph{the pointer value} on a fixed
location on the heap. This function can be used to store the root of a data
structure in a place that can easily be identified and read back.

\item | readRoot :: Heap (Ptr f a) |

The |readRoot| operation can be used to read back the pointer that has been
stored by the |writeRoot| operation. 

\end{itemize}

The |allocate| and |free| heap operations are both used in the
implementation of |write|, |read|, |fetch|, but are not used further
in this framework. In the next section we see how the five higher-level
functions |write|, |read|, |fetch|, |writeRoot| and |readRoot| can be
used in combination with the annotation framework to build persistent data
structures.

\subsection{Summary}

%if False

> read = undefined
> fetch = undefined
> write = undefined
> update = undefined
> free = undefined
> run = undefined

%endif

In this section, we have sketched the interface of a file based heap
structure. It can be used to store arbitrary blocks of binary data on disk.
Access to the data is managed by pointers as offsets into the file. All Haskell
values that have an instance for the |Binary| type class can automatically be
marshalled from and to the heap. The heap structure is low-level and does not assume
anything about the contents of the individual blocks.

