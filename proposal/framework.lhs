%include polycode.fmt

\section{The framework}

  \subsection{Data type generic programming}

  Data type generic programming in Haskell can be used to write functions to
  process values of different types.  Different levels of generality can be
  achieved by making more or less assumptions about the layout of data.  Making
  less assumptions about the structure of data means values of more data types
  can be processed by the same function.  By making more assumptions about the
  structure of data more knowledge can be exploited when processing it, but
  values of less data structures can be used.

  Finding the right level of abstraction is crucial when building a generic
  programs.  Several general views on data exist in Haskell, most of which use
  type classes to access them.  Examples of these are monoids, (applicative)
  functors and monads.  Others generic views can be obtained by abstracting
  away concrete patterns, like the fixed point view on data types which
  abstracts away from recursion.

  \subsection{Fixed point view on data types}

  Most container data structures used in computer science are based on
  recursive data types.  Recursively -- or inductively -- defined data types
  have a reference to itself in their definition, this way managing to store
  more than one element.

  To allow this persistence framework to cut large data structures into
  separate non-recursive pieces we assume a fixed point view on the data types
  we process.  By having an explicit notion of the recursion we can efficiently
  marshal the non-recurive pieces to disk without further knowledge of our
  domain.

  Abstracting away from recursion is a common pattern in generic programming
  and can be done by parametrizing data types with a recursive variable.  To
  illustrate this, take the following inductive data type:

>data Tree a = Leaf | Branch a (Tree a) (Tree a)

  By parametrizing the |Tree| data type with an additional type parameter for
  the recursive point we come up with the following data type:

>data FTree a f = Leaf | Branch a f f

  Applying a type level fixed point combinator to such an open data type gives
  us back something isomorphic to the original:

>data Fix f = In { out :: f (Fix f) }
>data FixTree a = Fix (FTree a)

  Opening up recursive data types as shown above is a rather easy and
  mechanical process that can easily be done automatically and generically
  using several generic programming libraries.

  The same is not the case for the recursive functions working on these data
  types.  Abstracting out the recursion in the function working on our
  recursive data structures demands some changes in the way a function is
  written.  Several techniques can be thought of to achieve this, all of which
  demands some changes in the way the developer writes the algorithms.

  \begin{itemize}

    \item

    Functions that explicitly go into recursion can be parametrized with an
    additional function that takes care of the recursion.  A fixed point
    combinator on the value level can be used to tie the knot.  This way of
    outsourcing the recursion to the caller using a fixed point combinator is
    similar to the trick on the type level.

    \item

    Functions can be described as algebras or coalgebras and be lifted to true
    catamorphism and anamorphisms using specialized folds and unfolds.  This
    technique is very well documented in literature and should be powerful
    enough to express all functions of our data types.

    \item
    
    Another option is to use program transformations to convert existing
    recursive function into open variants.  While this is possible in theory is
    requires a lot of meta programming to achieve this.

  \end{itemize}

  Because there are several ways to factor out the recursion from our functions
  that operate on the recursive data structures an interesting investigation
  will be to figure out what approach is the most transparent and easy to use.

  \subsection{Annotated fixed points}

  As we have seen above, when we abstract away all the recursive points in both
  our data structures and the function working on our data when can get back
  our original definitions by tying the knot using a fixed point combinator.
  The control over recursion is now shifted from the data structure to our
  framework which now enables us to generically annotate the behaviour.  The
  fixed point combinator at the data type level can be used to store additional
  information inside the recursive points of our recursive data types.  The
  fixed point combinator at the value level can be used to perform additional
  actions when the original function would otherwise have gone into recursion
  directly.  This way we can annotate the data structure with new functionality
  that changes the representation of our structures but not the way they
  algorithms work on it.

  \subsection{Persistent data}

  By annotating the recursive points of the data structure with pointers to
  locations in the storage heap instead of the real sub structures it can be
  made persistent on disk.  By annotating the recursive functions that operate
  on the data structure with additional actions that read or write the
  recursive structures from or to disk, you can also project the functions to
  operate on disk.  Because all the data structures and functions are written
  without the explicit recursion, all of this happens transparently to the
  writers of the data structure.

  \subsection{Storage heap}

  Mimicking what is going on in memory on disk requires a data structure tho
  handle the allocation and deallocation of blocks of persistent storage.  A
  heap just like the one in regular application memory with the ability to
  allocate, free and reuse blocks of data will fit our demands.  This heap will
  be stored in a single file on disk and should be a able to grow and shrink on
  demand.  To allow the algorithms used on data structures to perform on disk
  with the same asymptotic time and space complexity as in application memory,
  seeking for and reading data from a storage block should work in constant
  time.

  \subsection{Binary serialization}

  In order to store values of arbitrary data types to disk a tool is needed to
  generically serialize values to and deserialize from binary streams.  Generic
  binary serialization is well described in literature and can be done using
  several generic programming techniques.  Ideally users of this framework do
  not have to write their binary serialization code or a generic views on there
  data structures themselves.  Such boilerplate code should be taken care of by
  the generic programming library.

  \subsection{Summary}

  Transparently annotating purely functional data structures with additional
  data and additional functionality can be done by abstraction away from
  recursion in their definitions.  By annotating the recursive point in the
  data values with pointers to locations on a storage heap on disk and
  annotating the functions with actions to read and write the data from or to
  disk, we can transparently persist data structures on disk.  Because we cut
  the data structures into separate non-recursive pieces, the algorithms can
  work on persistent data without reading and writing the entire structure from
  or to disk.  Because we can freely navigate individual parts of the
  persistent structures the time and space complexity of the algorithms is the
  same as it would be when running in memory.  This makes this framework useful
  for the use of both general purpose and domain specific data structures.

