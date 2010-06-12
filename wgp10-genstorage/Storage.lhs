%if False

> {-# LANGUAGE
>     ScopedTypeVariables
>   , FlexibleInstances
>   , FlexibleContexts
>   , MultiParamTypeClasses
>   , UndecidableInstances
>   , GeneralizedNewtypeDeriving
>   #-}
> module Storage where

> import Control.Monad
> import Data.Binary
> import Data.Traversable
> import Fixpoints
> import Heap
> import Morphisms
> import Prelude hiding (read, lookup)

%endif

\section{Persistent data structures}
\label{sec:storage}

\todo{Intro: In section ? we have X and in section ? we have Y and now we combine.}

In the previous section we have deliberately build a |Ptr| type with to type
parameters, the functor type |f| with an explicit index |a|. Due to these two
type parameters the type becomes usable as a fixed point annotation. We can
make the |Ptr| type an instance of both the |Out| and |In| type class from
section \ref{sec:annotations}. We associate the pointer annotation with the |Heap|
type parameters the type becomes usable as a fixed point annotation. We build a
\emph{persistent} binary search tree by using a pointer annotation at the
recursive positions of the tree:

> type TreeP k v = FixA Ptr (TreeF k v)

When we work with a value of type |TreeP k v| we now actually work with a
\emph{pointer} to binary tree somewhere on the heap on disk. The pointer
references a block on the heap that stores a binary serialization of node with
type |TreeF k v (TreeP k v)|; a single node with at the recursive positions
again pointers to the sub structures.

\begin{figure*}[pt]
\begin{center}
\includegraphics[scale=0.3]{img/binarytree-pers.pdf}
\end{center}
\caption{A persistent binary tree that lives on the storage heap. Each node is
stored on its own heap block in binary representation. All sub structures are
referenced by pointer to the file offset.}
\label{fig:binarytree-pers}
\end{figure*}

\subsection{Persistent producers and queries}

We make the |Ptr| type an instance of both the |Out| and |In| type class from
section \ref{sec:annotations}. We associate the pointer annotation with the
|Heap| context and use the |read| operations as the implementation for |outA|
and use the |write| operation as the implementation for |inA|:

> instance (Traversable f, Binary (f (FixA Ptr f))) => Out Ptr f Heap
>    where outA = read
>
> instance (Traversable f, Binary (f (FixA Ptr f))) => In Ptr f Heap
>    where inA = write

To make the two instances work we need a |Binary| instance for both the fixed
combinator and the |TreeF| pattern functor. Both instances can be seen in
figure \ref{fig:binary-instances}.

\begin{figure}[pt]
\begin{center}

> instance Binary (f (Fix f)) => Binary (Fix f) where
>   put (In f) = put f
>   get = fmap In get 
>
> instance  (Binary k, Binary v, Binary f)
>       =>  Binary (TreeF k v f) where
>   put Leaf              = do  putWord8 0
>   put (Branch k v l r)  = do  putWord8 1
>                               put k; put v; put l; put r
>   get = do  t <- get
>             if t == (0 :: Word8)
>              then return Leaf
>              else liftM4 Branch get get get get

\end{center}
\caption{The |Binary| instances for the fixed point combinator |Fix| and the
|TreeF| pattern functor.}
\label{fig:binary-instances}
\end{figure}

We now specialize the |fromList| function from section \ref{sec:apomorphisms}
to use the pointer annotation in the |Heap| context. This yields a operation
that builds a binary search tree \emph{on disk} instead of in application
memory:

> fromListP :: [(Int, Int)] -> Heap (TreeP Int Int)
> fromListP = fromList

The result of running the operation against a heap file is a pointer, wrapped
inside an |In| construcotr, to the root node of the tree as stored on disk. 
Performin the operation is as simple as supplying it to the |run| function from
our heap data structure:

\begin{verbatim}
ghci> let squares = [(1,1),(3,3),(4,16),(7,49)]
ghci> run "squares.db" (fromListP squares)
\end{verbatim}

In figure \ref{fig:binarytree-pers} we see an illustration of our example tree
laid out on the heap. The example above does write a binary tree of integers to
disk as we expected, but has a slight problem when used on its own: the root
pointer of the structure is discarded and lost. We build a helper function
|produce| that takes a producer operation, like |fromListP|, runs the operation
on the heap \emph{and} save the final pointer in a reserved location on the
heap:

> produce :: Binary (f (Fix f)) => Heap (Fix f) -> Heap ()
> produce c = c >>= update (P 0) . out

By writing the pointer to the root of the produced functional data structure in
the special \emph{null block} we can easily relocate it to perform consecutive
operations on the same data structure. We remove the \texttt{squares.db} and
run the example again, this time saving the root node:

\begin{verbatim}
ghci> run "squares.db" (produce (fromListP squares))
\end{verbatim}

We make a similar wrapper function for performing query functions. The |query|
operation takes a heap operation and supplies it as input the data structure
pointed to by the pointer stored in the null block:

> query :: Binary (f (Fix f)) => (Fix f -> Heap b) -> Heap b
> query c = read (P 0) >>= c . In

We can now run any query operation on the binary tree of squares stored on
disk. We specialize the |lookup| function and apply it to our squares database:

> lookupP :: Int -> TreeP Int Int -> Heap (Maybe Int)
> lookupP = lookup

\begin{verbatim}
ghci> run "squares.db" (query (lookupP 3))
Just 9
\end{verbatim}

\todo{explain what happens}

\subsection{Persistent modification}

The previous section explained how to write both persistent producer functions
and persistent query function. Those operations either construct a new data
structure from a seed, or compute a result value from an existing data
structure. In this section we show how to write operations that modify existing
data structure on disk.

We start by giving an instance for the |OutIn| type class for the pointer
annotation in the |Heap| context:

> instance (Traversable f, Binary (f (FixA Ptr f))) => OutIn Ptr f Heap
>    where outInA f = write <=< f <=< fetch

We explicitly do not reuse the default implementation for |outInA|, which for
the |Ptr|/|Heap| instance would be equivalent to:

\begin{spec}
write <=< f <=< read
\end{spec}

Recall the |fetch| heap operation from section \ref{sec:heap}, after reading a
node from disk it immediately frees it.  By using |fetch| instead of |read| we
make all modification to the persistent data structure are \emph{in-place,
mutable updates}. \todo{only usable when being lazy}

With the |OutIn| instance we can now also project modification functions like
|insert| to work on the persistent storage:

> insertP :: Int -> Int -> TreeP Int Int -> Heap (TreeP Int Int)
> insertP = insert


> modify :: Binary (f (Fix f)) => (Fix f -> Heap (Fix f)) -> Heap ()
> modify c = read (P 0) >>= c . In >>= update (P 0) . out

\begin{verbatim}
ghci> run "squares.db" (query (lookupP 9))
Nothing
ghci> run "squares.db" (modify (insertP 9 81))
ghci> run "squares.db" (query (lookupP 9))
Just 81
\end{verbatim}

Using this building blocks we... \todo{blablabl}

Appendix a shows an example program.

