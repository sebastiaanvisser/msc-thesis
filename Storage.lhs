%include polycode.fmt
%include thesis.fmt

%if False

> {-# LANGUAGE
>     KindSignatures
>   , GeneralizedNewtypeDeriving
>   , FlexibleContexts
>   , UndecidableInstances
>   , FlexibleInstances
>   , MultiParamTypeClasses
>   #-}
> module Storage where

> import Control.Monad
> import Control.Monad.Trans
> import Data.Traversable
> import Data.Binary
> import Fixpoints
> import Morphisms
> import Heap
> import Prelude hiding (read)

%endif

\begin{chapter}{Generic storage}
\label{chap:storage}

In chapter \ref{chap:fixpoints} and \ref{chap:morphisms} we have built a
framework for generic traversals over annotated recursive data structures. In
chapter \ref{chap:heap} we have shown a disk based a storage heap that can be
used to store arbitrary blocks of binary data. In this chapter we show how
to combine our generic annotation framework with the storage heap to derive a
generic storage framework.

We use the |Pointer| type to create an annotation that maps non-recursive
nodes to individual storage blocks on the storage heap.  Using these
annotations we lift the operations working on the recursive datatypes to one
of the heap contexts. These lifted operation now no longer operate
in-memory but read there input, and write back their output, to an on-disk
representation.

\begin{section}{Pointer annotation}

In section \ref{sec:heaplayout} we have seen the |Pointer| type that stores an
offset into the storage heap. This pointer is represented by an integer value.
When we want to represent recursive datatypes that live on our storage heap, we
can use these pointers as the annotations for our fixed points. Using pointers
at the recursive positions slices a recursive data structure into individual
non-recursive pieces. Each piece is indirectly connected using a |Pointer|
instead of a regular in-memory connection.

The |Pointer| datatype itself cannot be used as an annotation because it has
kind |* -> *|, while the annotations in our framework have kind |(* -> *) -> (*
-> *)|. To be able to still use pointers as annotations we define a simple
wrapper type |P| with the right kind.

> newtype P f a = P { unP :: Pointer (f a) }
>  deriving Binary

We can now represent a persistent version of our binary tree with the following
type.

> type PersistentTree = FixA P Tree_f

The |PersistentTree| describes a recursive datatype, but the recursion is
indirect. All connections between |Branch| nodes to other |Branch| nodes or
|Leaf| nodes are described as pointer to locations on the heap. In figure
\ref{fig:treepers} we see an example of an binary annotated with the |P|
annotation.

\begin{figure}[bp]
\label{fig:treepers}
\begin{center}
\includegraphics[angle=90,scale=0.35]{./binarytree-pers.pdf}
\end{center}
\caption{This image shows the same binary tree as in figure
\ref{fig:binarytree} and figure \ref{fig:binarytree-ann}, but this time
annotated with the |Pointer| annotation and laid out on the storage heap. Every
node is stored in its own heap block and at a certain offset.  Every |Branch|
nodes refers to other node using the pointer annotation that stores the integer
offset of the heap block the target can be found in.
}
\end{figure}

\end{section}

\begin{section}{Annotation instances}

In chapter \ref{chap:morphisms} dealing with generic traversals over recursive
datatypes, we have implemented annotation aware paramorphism and apomorphism.
These traversal functions can be used to both destruct and construct recursive
datatype with annotated fixed points. For the binary tree example we seen some
simple algebras and coalgebras to describe the semantics of the traversal. In
order to be able to apply these algebras to our persistent binary tree we have
to make the |P| type an instance of the annotation type classes.

We first make |P| an instance of the |AnnO| type class. We can simply
use the |read| function from our read-only storage heap for the implementation.
This read-only annotation can be associated with the |HeapR| context. We first
unpack the fixed point and the |P| wrapper and than read one non-recursive node
from disk. Because the storage heap only stores plain strings of bits a
|Binary| instance is needed to deserialize the Haskell value. The |annO|
function takes a pointer to one non-recursive node, possibly containg pointers
to other nodes again, and return this node, with type |f (FixA P f)|. When the
fixed point does not contains an annotation at all we just unpack the node like
we did with the other annotations instances,

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnO P f HeapR where
>
>   annO (InA (P  f)  ) = read f
>   annO (InF     f   ) = return f

The |AnnI| instance for the |Pointer| type is just a matter of writing a
single non-recursive node to disk and storing the pointer inside the |P|
wrapper type. This action can only be done inside the read-write |HeapW|
context.

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnI P f HeapW where
>
>   annI = fmap (InA . P) . write

Sometimes, when working inside the read-write heap context |HeapW|, we also
want to be able to perform read-only actions, so we also give an |AnnO|
instance for the |Pointer| type inside the |HeapW| context. The implementation
is similar to the one for the read-only |HeapR| context, except we no lift
teh |read| operation to the read-write context.

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnO P f HeapW where
>
>   annO (InA (P  f)  ) = liftR (read f)
>   annO (InF     f   ) = return f

Because we now both have an |AnnO| and an |AnnI| instance for the |Pointer|
type inside the |HeapW| context we can create an instance for the |AnnIO|
typeclass. The |AnnIO| instance assumes the value that gets modified is not
needed again in its original form. We use the |fetch| function to
remove the original block from disk.

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnIO P f HeapW where
>   annIO f (InA (P  h)  ) = fmap (InA . P) (write =<< f =<< fetch h)
>   annIO f (InF     h   ) = fmap (InA . P) (write =<< f h)

In order to store the recursive structures on disk we also need a |Binary|
instance for the annotated fixed point operator type itself. This is a partial
instance because we do not allow to store unannotated fixed points. This
means the structure must first be fully annotated using the |fullyIn|
function.

> instance Binary (a f (FixA a f)) => Binary (FixA a f) where
>
>   put = put . outa
>   get = liftM InA get

These instances are the only thing we need to connect our generic annotation
framework to our storage heap implementation. In the next section we see
how we can use the building blocks described upto now to build and inspect a
true binary tree on disk.

\end{section}

\begin{section}{Persistent binary tree}
\label{sec:persistenttree}

In chapter \ref{chap:morphisms} we have implemented some example algebras and
coalgebras working on binary trees. These functions could be lifted to true
functions using the |paraMA| and |apoMA| functions. When lifted the function
work for an annotated binary tree in some monadic context. The context has been
associated with the annotation type using one of the annotation type classes.
Recall these four functions working on binary trees.

\begin{spec}
fromListMA  :: AnnI   a Tree_f m => [Int]           -> m (TreeA a)
containsMA  :: AnnO   a Tree_f m => Int -> TreeA a  -> m Bool
insertMA    :: AnnIO  a Tree_f m => Int -> TreeA a  -> m (TreeA a)
repminMA    :: AnnIO  a Tree_f m => TreeA a         -> m (TreeA a)
\end{spec}

Using the annotation instances for the |P| type we can now lift these four
function to work on persistent binary trees. The only thing we have to in order
to achieve this is specialize the types. The persistent versions of these
functions become:

%if False

> instance Binary (Tree_f a) where
>   get = undefined
>   put = undefined

%endif

> pfromList :: [Int] -> HeapW PersistentTree
> pfromList = fromListMA
>
> pcontains :: Int -> PersistentTree -> HeapR Bool
> pcontains = containsMA
>
> pinsert :: Int -> PersistentTree -> HeapW PersistentTree
> pinsert = insertMA
>
> prepmin :: PersistentTree -> HeapW PersistentTree
> prepmin = repminMA

The implementations can just be reused because the original functions are
generic enough to work for all annotations that have an instance for the
|AnnI|, |AnnO| and |AnnIO| classes. Note that the read-only functions are
lifted to the |HeapR| context, while the read-write functions are lifted to the
|HeapW| context.

These persistent operations can now be applied against the storage heap. We
define an example function that opens a heap file, builds an initial tree from
a list, inserts one item to the list and then replaces all the value with the
minimum value of the tree.

> buildTree :: FilePath -> IO ()
> buildTree file = run file $
>   produceP $  pfromList [5, 3, 2, 4, 1] >>=
>               pinsert 4 >>=
>               prepmin

The |produceP| function is a simple wrapper around |produce| (from section
\ref{sec:rootnode}) that works for the |PersistentTree| type, instead of the
|Pointer| type directly.

> produceP :: HeapW (FixA P f) -> HeapW ()
> produceP c = produce (liftM (unP . outa) c)

And, as a second example, we define a function that opens a heap file and
checks whether the tree stored on the heap contains the value 1. The boolean
result is printed to the standard output.

> inspectTree :: FilePath -> Int -> IO ()
> inspectTree file i = run file $
>   do  j <- queryP (pcontains i)
>       liftIO (print j)

The |queryP| function is a simple wrapper around |query| (also from section
\ref{sec:rootnode}) that works for the |PersistentTree| type, instead of the
|Pointer| type directly.

> queryP :: (FixA P f -> HeapR c) -> HeapW c
> queryP c = query (c . InA . P)

Now we can run these operations consecutively and see the expected result: the
tree written to disk contains the value 1 (which was the minimum when we
applied |prepmin|) and does not contain the value 3.

\begin{verbatim}
ghci> buildTree "tree.db"
ghci> inspectTree "tree.db" 2
True
ghci> inspectTree "tree.db" 3
False
\end{verbatim}

So we have specialized some generic operations working on annotated binary
trees to work for the |P| annotation in the heap contexts and were able to run
the operations with the |run| function from our storage heap. Although the
example invocations look simple and produce the expected result, in the
background a lot is going on. Let us try to explain what happens when running
these operations.


When we run the command \texttt{buildTree "tree.db"} the |run| function
opens up a file containing a storage heap and applies the given |HeapW|
operation to this heap. We now give a step-by-step overview of the
important steps that happen internally.

\begin{itemize}

\item
The |run| function first opens the specified file in read-write mode and
starts scanning the file. As explained in section \ref{sec:runheap} all blocks
are traversed to build up an in-memory allocation map. This map can be used
for the allocation of new blocks and freeing of existing blocks of binary data.

\item
After reading the allocation map the |run| function starts the actual heap
operation inside the wrapped |produce| function. Our operations is a monadic
sequencing of four operations, so the |HeapW| computation naturally starts with
the first, the |pfromList [5, 3, 2, 4, 1]|. Recall from section
\ref{sec:heapwrite} that the |HeapW| context internally wraps both the |HeapA|
context from section \ref{sec:heapalloc} and the |HeapR| context from section
\ref{sec:heapread}. This monad transformer stack internally provides the
allocation map and heap file handle to all operations.

\item
The |pfromList| function is built up from the |fromListCoalg|, which is lifted
to a true function using the apomorphic traversal |apoMA|. The definition of
|apoMA| in section \ref{sec:apomorphisms} shows it uses the coalgebra to
corecursively create a new structure. At every step in the recursive generation
of the structure the individually created nodes are wrapped inside an
annotation using the |annI| function.

In our case the annotation type is specialized to the |P| type. This means that
for every node that the |fromListCoalg| produces, the apomorphisms uses the
|annI| function for the |P| instance to wrap the node inside the |P| annotation.

Now recall the definition of the |AnnIn| instance for |P|.

\begin{spec}
annI = fmap (InA . P) . write
\end{spec}

This definition means that wrapping a node inside the |P| annotation means
writing the binary representation of the node to disk in a freshly allocated
block and storing the offset to that block in side the |P| constructor. For
example, the invocation of |annI Leaf| results in something like |InA (P
(Ptr 234))|. This implies that a value of type |FixA P Tree_f| only contains a
wrapped offset to a place on disk where the actual node can be found.

So, when the |pfromList| function is finished all the produced |Branch| and
|Leaf| nodes are stored in binary form, each on their own block, in the
storage heap. All |Branch| nodes have pointers at the recursive positions
to other heap blocks containing the sub-tree. Figure \ref{fig:treepers} shows
an example of a persistent binary tree. The result variable |p| now contains a
pointer to the root of the binary tree.

\item
The next operation is the |pinsert 4 p|, which insert the value 4 in the
binary tree created by the previous operation. This operation works slightly
different from the |pfromList| operation, because it works on an existing
binary tree. The |pinsert| functions is built up from the |insertCoalg| and
lifted to a true function using the endomorphic apomorphism |coendoMA|.

The implementation of |coendoMA| (see section \ref{sec:endoapo}) differs from
the implementation of |apoMA|, the |coendoMA| traversal uses an existing binary
tree as the input seed. This difference becomes clear from the definition, the
function uses the |annIO| modifier function. 

So the traversal gets as input an annotated structure, in our case a |FixA P
Tree_f|, and uses the pointer to read the actual node from disk. This node is
passed to the endomorphic coalgebra which produces either a new seed or a
new sub structure. When the coalgebra finishes the |AnnIO| ensures the
result is stored to disk again. The usage of the |AnnIO| function forces every
node that gets touched to be fetched (and also removed) from and saved to disk.
This behaviour is similar to what happens in regular in-memory update
functions: inserting a node into a binary tree requires a \emph{copy} of the
entire path up to the place where the new nodes gets inserted. All sub-trees
that are not needed for the inserting are not touched. This makes the
asymptotic running time of the persistent algorithms equal to that of the
regular in-memory variants.

So, when the |pinsert| function finishes it has inserted a new node into the
existing tree on disk. When doing this the entire path to this node has been
updated including the root node. All old nodes are removed and the freed blocks
can be reused. The new root node is returned and represents the new binary
tree.

\item
The next step is the |prepmin| function, which is also a modifier function. The
|prepmin| function uses more or less the same techniques as the |pinsert|
function, however |prepmin| is a combination of algebra (|minAlg|) and an
endomorphic algebra (|repAlg|). After running the |prepmin| algorithm the tree
on disk now only contains six times the number 1.

\item
When the previous operations are finished the root pointer of the final binary
tree is saved in the variable |r|. This variable is stored as the root of the
structure inside the null block by the surrounding |produce| function section
\ref{sec:rootnode}.

Now the run function terminates and the heap file is closed. The heap
contains a sliced binary serialization of a binary tree containing 6 times the
value 1.

\item
Now we run the second example |inspectTree|, which performs a read-only
operation inside the |query| function. The |query| function (from section
\ref{sec:rootnode}) read the root of the data structure from the null block and
supplies this to the specified operation. The |pcontains| function is used to
check for the existence of a value 1 inside the persistent binary tree.  The
paramorphic operation performs a traversal over the persistent binary tree and
soon figures out the value 1 is indeed stored inside the tree.  Assuming the
|containsAlg| here is lifted using the paramorphism resulting from section
\ref{sec:laziness} the traversal is lazy.

Both the |buildTree| and the |inspectTree| are two distinct functions that
individually open up the heap file and perform their operations. This means
that both function can be compiled to different programs that run consecutively
without any shared state except the storage heap.

\end{itemize}

\end{section}

\begin{section}{Overview}

In this chapter we have seen how to connect the low level interface of our
storage heap to our generic annotation framework. By creating annotation type
class instances for the |PointerA| annotation, we were able to derive a generic
storage framework. All generic operations, in our example for binary trees,
that are specialized to work for the |PointerA| annotation 
work for persistent data structures out of the box.

The original algebraic operations for our binary trees are annotation-unaware,
only when they are lifted and specialized they can be used to manage data
structures stored on disk.

Generically annotating algebraically defined operations to work in a persistent
computational context is the main achievement of this project. This achievement
makes it possible to write purely functional algorithms on purely function data
structures, but still be able to use them in a non-pure environment.

The framework defined here allows for a wide range of extensions, most of which
are not yet implemented or even described in this report. In chapter
\ref{chap:futurework} we describe some limitation of this system and
possible future extensions.

The current framework is generic because it works for all regular recursive
datatypes. Most, but not all, common functional data structures are regular
recursive datatypes. In the next chapter we show some example of recursive
data structures that cannot be used in our current framework. In the next chapter we also
show what is needed to extend the framework so it can be used for indexed
datatypes like GADTs.

\end{section}

\end{chapter}

