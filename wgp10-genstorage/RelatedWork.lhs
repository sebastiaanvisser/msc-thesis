%if False

> {-# LANGUAGE
>     ScopedTypeVariables
>   , KindSignatures
>   , FlexibleContexts
>   , FlexibleInstances
>   , TupleSections
>   , GADTs
>   , RankNTypes
>   , MultiParamTypeClasses
>   , TypeOperators
>   , TupleSections
>   #-}
> module RelatedWork where

> import Data.Ord
> import Data.List hiding (group)
> import Data.Monoid
> import Prelude hiding (mapM)
> import Control.Monad.Identity (Identity(..))
> import Control.Monad hiding (mapM, (<=<))
> import Data.Foldable
> import Data.Traversable
> import System.IO.Unsafe
> import Fixpoints hiding (Algebra, cata, lookup, Coalgebra, fromList)
> import Morphisms hiding (Algebra, cata, lookup, Coalgebra, fromList)

%endif

\section{Discussion and future work}\label{sec:discussion}

In this section, we discuss some subtleties of our approach. We also
point out current shortcomings and topics for future work.

\subsection{Laziness}
\label{sec:laziness}

The framework for working with annotated recursive datatypes uses type classes
to associate functionality with creating and removing annotations at the
recursive positions. These type classes have an associated context that allows
annotating and un-annotating structures to have monadic effects.
If the context is a \emph{strict} context, the
operations working on the recursive data structure become strict too. This
strictness can have a severe and unexpected impact on the running time of the
algorithms.

As an example, recall the |lookup| function on binary search trees as
discussed in Section~\ref{sec:catamorphisms}.
If we instantiate the
annotation to be the identity annotation, the operation performs an in-memory
lookup, traversing one path in the tree from the root to a leaf. If the tree
is properly balanced, this corresponds to a runtime of $O(\log n)$ where
$n$ is the size of the tree.

However, if we
instantiate the annotation to be the pointer annotation from
Section~\ref{sec:storage},
the lookup function runs inside the |Heap| monad which is
strict, because the underlying |IO| monad is strict. The strict bind operator for the |Heap|
monad makes the |lookupP| operation traverse the entire tree, i.e., to run in
$\Theta(n)$. The same happens if we use the modification time or debug annotation
which are both based on the |IO| monad as well.

Two possible solutions for this problem come to mind:
\begin{itemize}
\item We can change the algebras to run in a monadic context. Then the
patterns no longer have to precompute the results and pass them to the
algebra, but can pass computations. It becomes the responsibility of the
implementor of the algebras to explicitly evaluate the inputs that are
needed.
\item We can try to ensure that the operations in a \emph{lazy monadic context}.
When the context is lazy, the entire operations becomes lazy while the
algebras remain pure. We thus have to find a way to regain laziness in
strict contexts.
\end{itemize}
We have adopted the second option: We build our recursion patterns on top of
\emph{lazy monads}. We make a type class that can be used to lift monadic
computations to lazy computations:

> class Lazy m where
>   lazy :: m a -> m a

We make an instance for the |IO| monad by using |unsafeInterleaveIO|. This
function delays |IO| operations until they are actually required, possibly
discarding them if their results are never used:

> instance Lazy IO where
>   lazy = unsafeInterleaveIO

For monads that are already lazy, we can instantiate |lazy| to be the
identity function.

A new catamorphism can be built that uses invokes the |lazy| method just
before going into recursion:

> lazyCataA ::  (Out ann t m, Lazy m, Traversable t) =>
>               (t b -> b) -> FixA ann t -> m b
> lazyCataA phi = return . phi <=< mapM (_lazy . lazyCataA phi) <=< outA

%if False

> _lazy :: (Lazy m) => m a -> m a
> _lazy = lazy

%endif
The lazy catamorphism ensures that the monadic actions will only be performed when
the algebra requires the results. The type context tells us this catamorphism
is only applicable to monads that can be run lazily. We derive a new |lookup|
function using |lazyCataA|:

> lookup k = lazyCataA (lookupAlg k)

When we perform a lookup on the output of |myTree_a| -- specialized to the debug annotation --
we see a clear reduction in the number of steps needed to compute the answer:
\begin{verbatim}
ghci> lookup 4 it
("out",Branch 3 9 () ())
("out",Branch 4 16 () ())
Just 16
\end{verbatim}

We have solved the laziness problem for the storage heap specifically by
creating two separate heap contexts, a read-only context which uses lazy IO and
a read-write context that uses strict IO. The pointer instance for the |Out|
type class is now associated with the read-only context, the instance for the
|In| type class is associated with the read-write context.

To avoid any problems regarding lazy IO, we strictly force the entire result
values of consumer operations to ensure all side-effects stay within the |Heap|
context and cannot escape. Our operations are now lazy on the inside but appear
strict on the outside.

\subsection{Other data structures}

We have shown how to build a generic storage framework for recursive data
structures. As running example, we used binary search trees, but the same
technique can easily be applied to any regular datatype, i.e., all types
that can be expressed as a fixed point of a functor in terms of |FixA|.

Non-regular datatypes such as families of mutually recursive datatypes,
nested datatypes~\cite{nested} and indexed datatypes or generalized
algebraic datatypes (GADTs)~\cite{foundationsfor} cannot be expressed directly.
However, it is known that many non-regular datatypes can be expressed
in terms of a \emph{higher-order} fixed point
combinator~\cite{initial,foundationsfor,multirec} such as

> newtype HFix f ix = HIn (f (HFix f) ix)

We have extended our annotation framework to such a setting. Each of the
constructions described in the paper can be lifted to the more complex scenario,
but no code reuse is directly possible due to the more complicated kinds
in the higher-order situation.

A more in depth report about persistent indexed datatypes can be found is
provided by Visser~\cite{sebas}. He shows how represent finger
trees~\cite{fingertree}, a nested
data structure supporting efficient lookup and concatenation, as an indexed
GADT and use the higher-order storage framework to derive a persistent finger
tree. All the structural invariants we expect the finger tree to have are
encoded using the datatype indices.

\subsection{Sharing}

The storage framework as described works for finite data structures.
Finite data structures that use sharing can be stored on disk using our
framework, but because sharing in Haskell is not observable, shared substructures
will be duplicated in the heap. Storing shared values more than once can be a
serious space leak for datatypes that heavily rely on sharing.

Solution has been proposed to make sharing in Haskell observable \cite{sharing,
reify}. These solutions are often not very elegant, because they require some
form of reflection on the internal machinery of the compiler runtime.

It would be a useful extension to our framework to allow designers of
functional data structures to explicitly mark points at which sharing is
possible. Sharing markers can limit the amount of data used to store data
structures on disk and can even allow cyclic data structures to be saved in a
finite amount of space. Note that a data structure with explicitly marked
points of sharing fits nicely into our general framework of representing
data structures as annotated fixed points.

The current storage framework without explicit sharing does not require any
special form of garbage collection. The modification functions written with the
help of the |OutIn| type class will automatically clean up old nodes that are
no longer needed. The ability of explicit sharing would change this, a
modification cannot blindly free old nodes because they might be shared with
other parts of the data structure. The addition of explicit sharing this
requires support for garbage collection.

\subsection{Concurrency}

The current framework only allows sequential access to the persistent data
structures. Concurrent access currently would most certainly cause
undesirable effects. Parallel access to the same persistent data structure is a
topic for future research. We could benefit from in-memory transactions systems
like \emph{software transactional memory}~\cite{stm} to manage concurrent
threads to manipulate the same structure. Transactional in-memory caches to
persistent data structures have been shown useful before~\cite{tcache}.

Another approach to concurrent access is making the data structures immutable.
Using |read| instead of the |fetch| in the |OutIn| type class would yield a
framework were modification functions copy the original structures. Different
threads can now work on their own version of a data structure. To make this
approach practically usable we need full sharing between different versions of
the data structures and need a garbage collector to clean up versions that are
no longer used.

% -----------------------------------------------------------------------------

\section{Related work}\label{sec:relatedwork}

\subsection{Generic programming with fixed points}

Generic programming with fixed points is a well-explored area, both for regular
datatypes~\cite{genintro, polyp} and mutually recursive
datatypes~\cite{multirec}. Most generic programming approaches use fixed points
of nested sums of product, a view in which recursion, constructors, and
constructor fields of algebraic datatypes are represented. Our approach uses a
more limited view in which we only abstract away from recursion and has 
been shown useful in quite some situations.

Swierstra~\cite{alacarte} shows how the fixed point combinator can be used to
abstract away from recursion to extend data types with new constructions. Van
Steenbergen et al.~\cite{selections} show how to use generic programming with
annotated fixed points to store source position information in abstract syntax
trees.  Recursion patterns for working with non-regular recursive datatypes
have been described by Ghani and Johann~\cite{initial}.

% \andres[inline]{There seems to be something missing here: the general
% idea to use fixed points, has been used for many purposes, such as -- among
% other things -- to allow extension of the datatype (Garrigue, our rewriting
% paper).}

\subsection{Lazy IO}

Lazy IO in Haskell has many associated problems. Pure code processing values
origination from effectful computations can trigger side effects and
technically behave as impure code.  Kiselyov \cite{iteratee} describes
iteratee-based IO
as a solution for the lazy IO problem. Until now their approach has
only been shown useful for linear IO system, like processing a file line by
line. Iterators have a structure similar to algebras for list catamorphisms, it
is not sure whether the iteratee approach is extensible to different functor
types, like the tree base functor.

\subsection{Persistent storage in Clean}

In their paper \emph{Efficient and Type-Safe Generic Data Storage} Smetsers,
Van Weelden and Plasmeijer~\cite{clean} describe a generic storage framework
for the programming language Clean. Similar to our storage framework, they aim
at generically mapping functional data structures to a persistent storage on
disk. Using something similar to our storage heap -- they call this
\emph{Chunks} -- they are able to store individual parts of the data
structures on the disk without the need for reading and writing the entire
collection at once.

The major difference between their approach and ours is that they do not slice
the data structure at the recursive points but at the points where the actual
element values are stored. This means that
every record value is stored in its own
chunk, while they entire data structure itself is stored in one single chunk.
Updates of individual record values can now be done efficiently without
touching the entire collection, but for every structural change to the
collection the chunk containing the data structure itself -- they call this
the \emph{Root chunk} -- has to be read in and written back as a whole.

\subsection{Happstack State}

The \emph{Happstack} \cite{happsstate} project consist of Haskell web server
and a state framework. The state framework is called \emph{Happstack-State}.
It uses a record-based system in which users can add, delete, modify and
retrieve records of data on a database file. The system uses Template
Haskell meta-programming to automatically derive storage operations for custom datatypes. The
derivation of operations only works for monomorphic types which severly breaks
modularity.  Happstack State only allows storing record values and does not
allow using custom domain specific data structures.

