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

\begin{figure}[h]
\label{fig:binarytree}
\begin{center}
\includegraphics[scale=0.25]{img/heap.pdf}
\end{center}
\caption{An example of a binary tree.}
\end{figure}

In this section we introduce a block based heap data structure that is used to
allocate and use blocks of binary data on disk. The structure of the heap is
similar of in-memory heaps used by most programming languages to manage
dynamically allocated data.

> newtype Heap a =
>   Heap  (  ReaderT  Handle
>         (  StateT   (Size, AllocMap)
>            IO ) a )

where:

> type Offset  = Integer
> type Size    = Integer

%if False

>   deriving
>     ( Functor, Monad
>     , MonadIO
>     , MonadReader Handle
>     , MonadState  (Size, AllocMap)
>     )

> newtype Ptr (f :: * -> *) a = P Offset
>   deriving Binary

> type AllocMap = Map Offset Size

%endif

> read      :: Binary (f a)  =>  Ptr f a  -> Heap (f a)
> fetch     :: Binary (f a)  =>  Ptr f a  -> Heap (f a)
> update    :: Binary (f a)  =>  Ptr f a -> f a          -> Heap ()
> write     :: Binary (f a)  =>  f a          -> Heap (Ptr f a)
> allocate  ::               Integer    -> Heap (Ptr f a)
> free      ::               Ptr f a  -> Heap ()
> run       :: FilePath  ->  Heap a     -> IO ()

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

