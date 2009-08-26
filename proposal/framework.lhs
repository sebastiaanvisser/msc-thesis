%include polycode.fmt

%if False

>{-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances #-}
>import Control.Monad
>import Control.Applicative

%endif

%format :.: = "\circ"

\section{The framework}

Transparently annotating purely functional data structures with additional data
and additional functionality can be done by abstracting away from the recursion
in their definitions.  By annotating the recursive point in the data values
with pointers to locations on a storage heap on disk and annotating the
functions with actions to read and write the data from or to disk, we can
transparently persist data structures on disk.  Cutting the data structures
into separate non-recursive pieces allows the algorithms to work on persistent
data without reading or writing the entire structure from or to disk at once.
The time and space complexity of the algorithms remains equivalent to the
original version running in memory, because we can freely access individual
parts of the persistent data structure.  Because the complexity of the
algorithms working on persistent data structures remains equivalent to their
in-memory counterparts, the framework is useful for persistence of both general
purpose and domain specific data structures.

We will now sketch the technical outline on how to use generic programming
techniques to annotate the behaviour of functional data structures.  Generic
annotation will be the basis of the persistence framework.  Annotation of data
structures is a very general technique that is not solely useful for data
persistence. Other project may benefit from generic annotation as well.  The
goal of this project includes research to the exploitation of generic
annotation.

\subsection{Datatype generic programming}

Datatype generic programming in Haskell can be used to write functions to
process values of different types.  Different levels of generality can be
achieved by making more or less assumptions about the layout of data.  Making
less assumptions about the structure of data means values of more datatypes
can be processed by the same function.  By making more assumptions about the
structure of data more knowledge can be exploited when processing it, but
values of less data structures can be used.

Finding the right level of abstraction is crucial when building generic
programs.  Haskell type classes can be used to do some form of generic
programming by providing different implementations for common concepts based on
the types involved. Examples of these type classes are monoids, (applicative)
functors and monads.  Data type generic programming can be used to write
functions that work for all types by exploiting the structure of algebraic
datatypes. By only looking at structural properties like the constructors,
fields, type variables and recursion, generic functions can be written that
make no assumptions about the actual datatypes.  Abstracting away from the
recursive points in a datatype is sometimes called the fixed point view on
datatypes and is really useful to this project.

\subsection{Fixed points of datatypes}

Most container data structures used in computer science are based on recursive
datatypes.  Recursively -- or inductively -- defined datatypes contain at
least one reference to itself in their definition, this way managing to store
more than one element.

To allow the persistence framework to cut large data structures into separate
non-recursive pieces we assume a fixed point view on the datatypes we process.
By having an explicit notion of the recursive positions in the data we can
efficiently marshal the non-recursive pieces to disk without further knowledge
of our domain.

Abstracting away from recursion is a common pattern in the field of generic
programming and can be done by parametrizing datatypes with a recursive
variable.  To illustrate this, we take the following inductive datatype that
represents a binary search tree.  Every |Tree| is either a |Leaf| or a |Branch|
with one value and two sub trees:

>data Tree a = Leaf | Branch a (Tree a) (Tree a)

%if False

>  deriving Show

%endif

By parametrizing the |Tree| datatype with an additional type parameter for the
recursive points we come up with the following datatype:

>data TreeF a f = LeafF | BranchF a f f

%if False

>  deriving Show

%endif

Applying a type level fixed point combinator to such an open datatype gives us
back something isomorphic to the original:

>newtype Fix f = In { out :: f (Fix f) }

%if False

>instance Show (f (Fix f)) => Show (Fix f) where
>  show = ("[| " ++) . (++ " |]") . show . out

%endif

>type FixTree a = Fix (TreeF a)

Opening up recursive datatypes as shown above is a rather easy and mechanical
process that can easily be done automatically and generically using several
generic programming libraries.

The same is not the case for the recursive functions working on these data
types.  Abstracting out the recursion in the function working on our recursive
data structures demands some changes in the way a function is written.  Several
techniques can be thought of to achieve this, all of which demands some changes
in the way the developer writes the algorithms.  Three possible ways of
abstraction away from recursion in function definitions are:

\begin{itemize}

\item

Functions that explicitly go into recursion can be parametrized with an
additional function that takes care of the recursion.  A fixed point combinator
on the value level can be used to tie the knot.  This way of outsourcing the
recursion to the caller using a fixed point combinator is similar to the trick
on the type level.

\item

Functions can be described as algebras or coalgebras and be lifted to
catamorphisms, anamorphisms, and paramorphisms using specialized folds and
unfolds.  This technique is very well documented in literature\cite{bananas}
and should be powerful enough to express all functions on our data
types\cite{paramorphisms}.

\item

Program transformations can be used to convert existing recursive function into
open variants.  While this is possible in theory is requires a lot of meta
programming to achieve this, e.g. using Template Haskell\cite{th}.  The main
advantage of this technique is that this can in theory be used to persist
\emph{existing} Haskell container datatypes.  The main disadvantage is that
this rather ad-hoc meta programming approach is not very easy and elegant.

\end{itemize}

So there are several ways to factor out the recursion from the functions that
operate on the inductive datatypes and it is not inherently clear which of
these is the best.  An interesting research topic for this project will be to
figure out what approach is the most transparent and easy to use.

\subsection{Annotated fixed points}

As illustrated above, when abstracting away from all the recursive positions in
both the datatypes and the functions working on them, the original definitions
can be obtained again by tying the knot using a fixed point combinator.  The
control over recursion is now shifted away from the definitions to the
framework.  This can be used to generically annotate the behaviour.

The fixed point combinator at the datatype level can be used to store
additional information inside the recursive points of these datatypes.  This
can be done using an annotated fixed point combinator.  First we have to define
composition on type level.

>infixr :.:
>newtype (f :.: g) a = C { unC :: f (g a) }

%if False

>  deriving Show

%endif

The annotated fixed point can now be defined by embedding a composition of an
open container with an annotation inside the fixed point combinator.

>type AnnFix f ann = Fix (f :.: ann)

The fixed point combinator at the value level can be used to perform additional
actions when the original function would otherwise have gone into recursion
directly.  One way to do this is to write an \emph{open} and \emph{lifted}
variant of such a function, like the following example of the function |count|
on our |Tree| datatype.  The |Query| type synonym is used to hide some details.

>type Query g f m c = (f -> m c) -> g f -> m c

>count :: Applicative g => Query (TreeF a) f g Int
>count _    LeafF            = pure 0
>count rec  (BranchF _ l r)  = (\a b -> a + b + 1) <$> rec l <*> rec r

%if False

> -- $

%endif

This function is lifted to operate inside \emph{some} applicative functor and
is open because it is parametrized with an additional function \emph{rec} that
takes care of the recursion.  By using a specialized fixed point combinator we
can tie the knot and perform an additional task where the function recurses.
For example, we can print out the sub structures we are processing:

>makePrintingQuery
>  ::  Show (f (Fix f))
>  =>  Query f (Fix f) IO c
>  ->  Fix f -> IO c
>makePrintingQuery q f = liftM out (m f) >>= w
>  where  w    = q (m >=> w . out)
>         m a  = print a >> return a

With this function we can annotate all open lifted query functions to print out
all intermediate sub structures.  Note that this function does not assume any
annotations in the recursive points, something which is in practice possible.
We can now annotate the |count| function, this now becomes:

>printCount :: Show a => FixTree a -> IO Int
>printCount = makePrintingQuery count

This way we can annotate the data structure with new functionality that changes
the representation of our structures but not the way we write the original
algorithms.

\subsection{Persistent data}

By annotating the recursive points of the data structure with pointers to
locations in a file-based storage heap instead of the real sub structures,
individual parts can be made persistent on disk without losing the connection
to sub structures.  Every non-recursive part of the structure can be handled
individually without having the entire structure in memory.  By annotating the
recursive functions that operate on the data structure with additional actions
that read the recursive structures from or write them to disk, you can also
project the functions to operate on disk.  Because all the data structures and
functions are written without the explicit recursion, all of this happens
transparently to the writers of the data structure.

\subsection{Binary serialization}

In order to store values of arbitrary datatypes to disk a tool is needed to
generically serialize values to and deserialize from binary streams.  Generic
binary serialization is well described in literature and can be done using
several generic programming techniques\cite{databinary, derive, emgm, syb,
multirec, compgen, printparse, clean}.  Ideally users of this framework do not
have to write their binary serialization code and generic views on their data
structures themselves.  Such boilerplate code should be taken care of by the
generic programming library.  The section on related work gives a more detailed
view of possible techniques.

\subsection{Storage heap}

Mimicking what is going on in memory on disk requires a data structure to
handle the allocation and deallocation of blocks of persistent data.  A heap
just like the one in regular application memory with the ability to allocate,
free and reuse blocks of data will fit our demands.  This heap will be stored in
a single file on disk and should be a able to grow and shrink on demand.

The interface of the heap should at least contain the following functions:

>store     :: Binary a =>  a ->               Storage t (Pointer a)
>retrieve  :: Binary a =>  Pointer a ->       Storage t a
>delete    ::              Pointer a ->       Storage t ()
>reuse     :: Binary a =>  Pointer a -> a ->  Storage t (Pointer a)

All these functions operate inside the |Storage| monad, which runs inside the
|IO| monad and has access to the underlying heap structure using a |State|
monad.  There is quite some freedom in the exact implementations of these
functions which may significantly affect performance.  We expect all of these
function to run with the same time and space complexity as their in-memory
equivalents, so the expected running time of persistent algorithms matches the
running time of the in-memory algorithms.

The |Store| function takes a value of any type that we can generically
serialize to a binary representation and allocates a fresh block on the heap,
stores the binary representation and returns a pointer to this block.  The
|Pointer| datatype is indexed with the type that is stored so we can later on
use it only to read back the value of the correct type.  The |Retrieve|
function takes pointer to a value of some type that we know of we can
deserialize from a binary representation and reads the value from the heap.
Internally it will read the binary representation from the heap and deserialize
the stream to a real value of the right type.  The |delete| functions frees an
existing block, making the previously occupied space available for future
allocations.  The |reuse| functions tries to reuse the existing space for a new
value when possible, or reallocates a new block when the existing block does
not contain enough space.

