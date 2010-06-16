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

> import Control.Monad hiding ((<=<))
> import Data.Binary
> import Data.Traversable
> import Fixpoints hiding (fromList, lookup, insert)
> import Heap
> import Morphisms
> import Prelude hiding (read, lookup)

%endif

\section{Persistent data structures}
\label{sec:storage}

In the previous section, we have chosen to define the |Ptr| datatype with
\emph{two} type parameters -- a functor of kind~|* -> *| and an explicit
index of kind~|*|. Due to this design decision, the pointer type becomes
usable as a fixed point annotation -- as we will see in Section~\ref{sec:ppq},
we can make the |Ptr| type an instance of both the |Out| and the |In|
type classes from Section~\ref{sec:annotations}. The effects associated with
wrapping an unwrapping live in the |Heap| monad. 

We then show how to build \emph{persistent} data structures such as binary
search trees, by using the pointer annotation:

> type TreeP k v = FixA Ptr (TreeF k v)

When we work with a value of type |TreeP k v| we now actually work with a
\emph{pointer} to a binary tree that lives somewhere on the heap that is
stored on the disk. To be precise, the pointer references a heap block that
stores a binary serialization of a single node of type |TreeF k v (TreeP k v)|.
The recursive positions of the node contain again pointers to substructures.
Figure~\ref{fig:binarytree-pers} shows how such a tree will look like.

\begin{figure*}[pt]
\begin{center}
\includegraphics[scale=0.3]{img/binarytree-pers.pdf}
\end{center}
\caption{A persistent binary tree that lives on the storage heap. Each node is
stored on its own heap block in binary representation. All sub structures are
referenced by pointer to the file offset.}
\label{fig:binarytree-pers}
\end{figure*}

\subsection{Persistent producers and consumers}\label{sec:ppq}

To make the pointer type |Ptr| usable as an annotation, we have to define
instances of the |Out| and |In| type classes from Section~\ref{sec:annotations}.
We associate the pointer annotation with the
|Heap| context, use the |read| operation as the implementation for |outA|
and use the |write| operation as the implementation for |inA|:

> instance  (Traversable f, Binary (f (FixA Ptr f))) =>
>           Out Ptr f Heap
>    where outA = read <=< return . out
>
> instance  (Traversable f, Binary (f (FixA Ptr f))) =>
>           In Ptr f Heap
>    where inA = return . In <=< write

To make the two instances work, we need a |Binary| instance for both the fixed
combinator and the |TreeF| pattern functor. Both instances are shown in
Figure~\ref{fig:binary-instances}.

\begin{figure}[pt]
\begin{center}
\texths

> instance Binary (f (Fix f)) => Binary (Fix f) where
>   put (In f) = put f
>   get = fmap In get 
>
> instance  (Binary k, Binary v, Binary f) =>
>           Binary (TreeF k v f) where
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

We can now specialize the |fromList| function from Section~\ref{sec:apomorphisms}
to use the pointer annotation in the |Heap| context. This yields a operation
that builds a binary search tree \emph{on disk} instead of in application
memory:

> fromListP :: [(Int, Int)] -> Heap (TreeP Int Int)
> fromListP = fromList

Note that all we have to do is to specialize the type of the original
operation. We can reuse exactly the same |fromList| function.

The result of running the |fromList| operation against a heap file is a pointer
to the root node of the tree as stored on disk, wrapped inside an |In|
constructor. Performing the operation is as simple as supplying it to the |run|
function from our heap interface:
\begin{verbatim}
ghci> let squares = [(1,1),(3,9),(4,16),(7,49)]
ghci> run "squares.db" (fromListP squares)
\end{verbatim}

Figure \ref{fig:binarytree-pers} shows an illustration of our example tree
laid out on the heap. The example above does write a binary tree of integers to
disk as we expected, but has a slight problem when used on its own: the root
pointer of the structure is discarded and lost. We therefore
define a helper function~|produce| that takes a producer operation, such as
|fromListP|, runs the operation on the heap and then saves the final pointer
in a reserved location on the heap:

> produce :: Binary (f (Fix f)) => Heap (Fix f) -> Heap ()
> produce c = c >>= update (P 0) . out

By writing the pointer to the root of the produced functional data structure in
the special \emph{null block} we can easily locate it again to perform
consecutive operations on the same data structure.
We delete the @squares.db@ file and
run the example again, this time saving the root node:
\begin{verbatim}
ghci> run "squares.db" (produce (fromListP squares))
\end{verbatim}

We make a similar wrapper function for performing consumer functions. The~|consume|
operation takes a heap operation and passes it as input the data structure
pointed to by the pointer stored in the null block:

> consume :: Binary (f (Fix f)) => (Fix f -> Heap b) -> Heap b
> consume c = read (P 0) >>= c . In

We can now run any consumer operation on the binary tree of squares stored on
disk. We specialize the |lookup| function and apply it to our squares database:

> lookupP :: Int -> TreeP Int Int -> Heap (Maybe Int)
> lookupP = lookup

\begin{verbatim}
ghci> run "squares.db" (consume (lookupP 3))
Just 9
\end{verbatim}

The database file is opened and the root pointer is read from the null block.
The root pointer references a persistent binary tree that is passed to the
|lookupP| function that, node by node, traverses the three until the key is
found and the value can be returned.

\subsection{Persistent modification}

We have described how producers such as |fromList| and consumers such as
|lookup| can easily be lifted to a persistent setting if defined in our
generic annotation framework. We now show the same for modifiers.

To start, we have to give an instance for the |OutIn| type class for
the pointer annotation in the |Heap| context:

> instance  (Traversable f, Binary (f (FixA Ptr f))) =>
>           OutIn Ptr f Heap
>    where outInA f = return . In <=< write <=< f <=< fetch <=< return . out

Note that we do \emph{not} use the default implementation for |outInA|,
which in this case would use |read| instead of |fetch|.
As explained in Section~\ref{sec:writeread},
|fetch| immediately frees a block after reading it. By using |fetch| instead
of |read|, we get the effect that all modifications to the persistent data
structure are \emph{mutable} operations. After a modification finishes the old
structure is no longer available.

With the |OutIn| instance we can now also specialize modification functions
such as |insert| to work on the persistent storage:

> insertP :: Int -> Int -> TreeP Int Int -> Heap (TreeP Int Int)
> insertP = insert

Similar to |produce| and |consume|, we define a function |modify|
that applies a given modifier to the tree pointed at by the pointer
in the null block, and stores the resulting tree pointer in the null
block once more:

> modify :: Binary (f (Fix f)) => (Fix f -> Heap (Fix f)) -> Heap ()
> modify c = read (P 0) >>= c . In >>= update (P 0) . out

Here is an example:
\begin{verbatim}
ghci> run "squares.db" (consume (lookupP 9))
Nothing
ghci> run "squares.db" (modify (insertP 9 81))
ghci> run "squares.db" (consume (lookupP 9))
Just 81
\end{verbatim}

This is an interesting example because it shows three consecutive runs on the
same database file. The second run modifies the binary tree on disk and stores
the new root pointer in the null block. A lookup in the third command shows us
the database file is updated.

\subsection{Summary}

This section we have combined the annotated recursion patterns and the basic
heap operations to derive persistent data structures. By annotating the
recursive datatypes with a pointer annotation we are able to store individual
non-recursive nodes on their own block on the heap. The |Out| and |In|
instances for the pointer type class |read| nodes from and |write| nodes to the
blocks on disk.

The operations on persistent data structures are applied to the file based
storage heap in the same way they are normally applied to the in-memory heap.
By writing data structures as pattern functors and by abstracting away from the
recursion we can annotate the behaviour generically. There is no need to
reflect over memory layout of the compiler runtime.

