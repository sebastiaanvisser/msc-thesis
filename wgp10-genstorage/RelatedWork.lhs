\section{Further work}

\subsection{Laziness}
\label{sec:laziness}

The framework for working with annotated recursive datatypes uses type classes
to associate functionality with wrapping and unwrapping annotations at the
recursive positions. These type classes have an associated context that allows
working with effects. When the context is a \emph{strict} context the
operations working on the recursive data structure become strict too. This
strictness can have severe implication on the running time of the algorithms.

Take the |lookup| function on binary search trees. When we instantiate the
annotation to be the identity annotation the operations performs an in-memory
lookup in an expected $O(\text{log }n)$ asymptotic running
time\footnote{Assuming the tree is a properly |balanced| binary search tree}.
When we instantiate the annotation to be the pointer annotation from section
\ref{sec:storage} the lookup function runs inside the |Heap| monad which is
strict due to both the strict |State| and strict |IO| monad. The strict bind
operator for the |Heap| monad makes the |lookupP| operation run in a mere
$O(n)$. Every node is touched in the process.

We have solved this problem by creating two separate heap contexts, a read-only
context which uses lazy IO and a read-write context that uses strict IO. The
pointer instance for the |Out| type class is now associated with the read-only
context, the instance for the |In| type class is associated with the read-write
context. The instance for the |OutIn| uses a hybrid approach, lifting lazy read
actions into the strict context. The separation between the two context allows
us to have strict producer functions and lazy query functions. The running time
of the persistent |lookup| function in the lazy context is reduced to the same
as its in-memory variant.

To avoid any problems regarding lazy IO, we strictly force the entire result
values of query operations to ensure all side-effects stay within the Heap
context and cannot escape. Our operations are now lazy on the inside but appear
strict on the outside.

\subsection{Sharing}

\todo{Sharing}

\subsection{Higher Order}

We have shown how to build a generic storage framework for recursive data
structures. This framework only works for regular datatypes, types in which the
recursive positions can only refer to the exact same type again. The system
does not work for any-non regular datatypes like mutually recursive datatypes,
nested datatypes \cite{nested} and indexed datatypes like generalized algebraic
datatypes or GADTs \cite{foundationsfor}.

As part of the reseach we have extended the framework for fixed point
annotations to also work for indexed datatypes. The first step in extending the
framework is to define a \emph{higher order} fixed point combinator:

> newtype HFix f ix = HIn (f (HFix f) ix)

The additional index parameter makes the recursion non-regular which allows us
to customize behaviour for different recursive positions. The techniques used
for generic programming with fixed points for indexed datatypes is well
described by Rodriguez et al. \cite{multirec}. Altough the annotation framework
for indexed datatypes is very similar to the framework for regular recursive
datatypes no code reuse is possible due to the difference in types.

In XXX \todo{refer to thesis somehow?} we show how represent a finger tree
\cite{fingertree} as an indexed GADT and use the higher order storage framework
to derive a persistent finger tree. All the structural invariants we expect the
finger tree to have are encoded using the datatype indices.

\section{Related work}

\subsection{Generic storage in Clean}

In their paper \emph{Efficient and Type-Safe Generic Data Storage} Smetsers,
Van Weelden and Plasmeijer \cite{clean} describe a generic storage framework
for the programming language Clean. Similar to our storage framework, they aim
at generically mapping functional data structures to a persistent storage on
disk. Using something similar to our storage heap -- they call this
\emph{Chunks} -- they are able to persist individual parts of the data
structures on disk without the need for reading and writing the entire
collection at once.

The mayor difference between their approach and ours is that they do not slice
the data structure at the recursive points but at the points where the actual
element values are stored. This means every record value is stored in its
own chunk, while they entire data structure itself is stored in one single
chunk. Updates of individual record values can now be done efficiently without
touching the entire collection, but for every structural change to the
collection the chunk containing the data structure itself --- they call this
the \emph{Root chunk} --- has to be read in and written back as a whole.


