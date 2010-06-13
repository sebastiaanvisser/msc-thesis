\section{Discussion and future work}

In this section, we discuss some subtle points of our approach. We also
point out current shortcomings and topics for future work.

\subsection{Laziness}
\label{sec:laziness}

The framework for working with annotated recursive datatypes uses type classes
to associate functionality with wrapping and unwrapping annotations at the
recursive positions. These type classes have an associated context that allows
annotating and un-annotating structures to have monadic effects.
If the context is a \emph{strict} context, the
operations working on the recursive data structure become strict too. This
strictness can have a severe and unexpected impact on the running time of the
algorithms.

As an example, consider the |lookup| function on binary search trees.
If we instantiate the
annotation to be the identity annotation, the operation performs an in-memory
lookup, traversing one path in the tree from the root to a leaf. If the tree
is properly balanced, this corresponds to a runtime of $O(\log n)$ where
$n$ is the size of the tree.

However, if we
instantiate the annotation to be the pointer annotation from
Section~\ref{sec:storage},
the lookup function runs inside the |Heap| monad which is
strict, because the underlying |IO| monad is strict.\andres{You said that
|State| is strict, too, but I don't think so.} The strict bind
operator for the |Heap| monad makes the |lookupP| operation traverse
the entire tree, i.e., to run in $\Theta(n)$.

We have solved this problem by creating two separate heap contexts, a read-only
context which uses lazy IO and a read-write context that uses strict IO. The
pointer instance for the |Out| type class is now associated with the read-only
context, the instance for the |In| type class is associated with the read-write
context. The instance for the |OutIn| uses a hybrid approach, lifting lazy read
actions into the strict context. The separation between the two context allows
us to have strict producer functions and lazy query functions. The running time
of the persistent |lookup| function in the lazy context is reduced to the same
as its in-memory variant.\andres{Quite vague. Reread and perhaps rewrite.}

To avoid any problems regarding lazy IO, we strictly force the entire result
values of query operations to ensure all side-effects stay within the Heap
context and cannot escape. Our operations are now lazy on the inside but appear
strict on the outside.

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

\todo[inline]{Sharing}

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

In XXX \todo{refer to thesis somehow?}\andres{Yes, give a link for the time
being.}, we show how represent finger trees~\cite{fingertree}, a nested data
structure supporting efficient lookup and concatenation, as an indexed GADT
and use the higher-order storage framework
to derive a persistent finger tree. All the structural invariants we expect the
finger tree to have are encoded using the datatype indices.

\subsection{Garbage collection}

\andres[inline]{TODO}

\subsection{Concurrency}

\andres[inline]{TODO}

\section{Related work}

\subsection{Generic programming with fixed points}

Generic programming with fixed points is a well-explored area, both for regular
datatypes~\cite{polyp}\andres{Cite Backhouse?}
and mutually recursive datatypes~\cite{multirec}. Most
generic programming approaches use fixed points of nested sums of product, a
view in which recursion, constructors, and constructor fields of algebraic
datatypes are represented. Our approach uses a more limited view in which we
only abstract away from recursion. The nested sums of product view is useful
when writing operations that are truly datatype generic. Only abstracting away
from recursion has shown to be useful when generically annotating datatype
specific operations.\andres{Might need a rewrite. ``Generic'' has been
used in our sense before, which is good.}

Recursion patterns for working with non-regular recursive datatypes have been
described by Ghani and Johann~\cite{initial}.

\andres[inline]{There seems to be something missing here: the general
idea to use fixed points, has been used for many purposes, such as -- among
other things -- to allow extension of the datatype (Garrigue, our rewriting
paper). Martijn uses annotations of recursive structures to store line
number information.}

\subsection{Lazy IO}

Lazy IO in Haskell has many associated problems. Pure code processing values
origination from effectful computations can trigger side effects and
technically behave as impure code.  Kiselyov \cite{iteratee} describes
iteratee-based IO
as a solution for the lazy IO problem. Until now their approach has
only been shown useful for linear IO system, like processing a file line by
line. Iterators have a structure similar to algebras for list catamorphisms,
whether their approach is extensible to different functor types is still a
topic of active research.\andres{Is it? References? Backup?}

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

\subsection{Happstack state}

The \emph{Happstack} \cite{happsstate} project consist of Haskell web server
and a state framework. The state framework is called \emph{Happstack-State}.
It uses a record-based system in which users can add, delete, modify and
retrieve records of data. This framework uses meta-programming using Template
Haskell to automatically derive storage operations for custom datatypes. The
derivation of operations only works for monomorphic types which severly breaks
modularity.  Happstack State only allows storing record values and does not
allow using custom domain specific data structures.

