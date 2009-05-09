%include polycode.fmt

%if False

>{-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances #-}
>import Control.Monad
>import Control.Applicative

%endif

%format :.: = "\circ"

\section{The framework}

  \fancy{
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
  }

  \fancy{
  This section sketches the technical outline on how to use generic programming
  techniques to annotate the behaviour of functional data structures. Generic
  annotation will be the basis of the persistence framework.
  }

  \fancy{
  The techniques described in this proposal have far more practical
  implications than will described in this section. The goal of this project
  includes research to the exploitation of this technique.
  }

  \subsection{Data type generic programming}

  \fancy{
  Data type generic programming in Haskell can be used to write functions to
  process values of different types.  Different levels of generality can be
  achieved by making more or less assumptions about the layout of data.  Making
  less assumptions about the structure of data means values of more data types
  can be processed by the same function.  By making more assumptions about the
  structure of data more knowledge can be exploited when processing it, but
  values of less data structures can be used.
  }

  \fancy{
  Finding the right level of abstraction is crucial when building a generic
  programs.  Several generic views on data exist in Haskell, most of which use
  type classes to access them.  Examples of these are monoids, (applicative)
  functors and monads.  Others generic views can be obtained by abstracting
  away concrete patterns, like the fixed point view on data types which
  abstracts away from recursion.
  }

  \subsection{Fixed point view on data types}

  \fancy{
  Most container data structures used in computer science are based on
  recursive data types.  Recursively -- or inductively -- defined data types
  contain at least one reference to itself in their definition, this way
  managing to store more than one element.
  }

  \fancy{
  To allow the persistence framework to cut large data structures into separate
  non-recursive pieces we assume a fixed point view on the data types we
  process.  By having an explicit notion of the recursion we can efficiently
  marshal the non-recursive pieces to disk without further knowledge of our
  domain.
  }

  \fancy{
  Abstracting away from recursion is a common pattern in the field of generic
  programming and can be done by parametrizing data types with a recursive
  variable.  To illustrate this, we take the following inductive data type that
  represents a binary search tree. Every |Tree| is either a |Leaf| or a
  |Branch| with one value and two sub-|Tree|s:
  }

>data Tree a = Leaf | Branch a (Tree a) (Tree a)

%if False

>  deriving Show

%endif

  \fancy{
  By parametrizing the |Tree| data type with an additional type parameter for
  the recursive point we come up with the following data type:
  }

>data FTree a f = FLeaf | FBranch a f f

%if False

>  deriving Show

%endif

  \fancy{
  Applying a type level fixed point combinator to such an open data type gives
  us back something isomorphic to the original:
  }

>data Fix f = In { out :: f (Fix f) }

%if False

>instance Show (f (Fix f)) => Show (Fix f) where
>  show = ("[| " ++) . (++ " |]") . show . out

%endif

>type FixTree a = Fix (FTree a)

  \fancy{
  Opening up recursive data types as shown above is a rather easy and
  mechanical process that can easily be done automatically and generically
  using several generic programming libraries.
  }

  \fancy{
  The same is not the case for the recursive functions working on these data
  types.  Abstracting out the recursion in the function working on our
  recursive data structures demands some changes in the way a function is
  written.  Several techniques can be thought of to achieve this, all of which
  demands some changes in the way the developer writes the algorithms. Three
  possible ways of abstraction away from recursion in function definitions are:
  }

  \begin{itemize}

    \item
    \fancy{
    Functions that explicitly go into recursion can be parametrized with an
    additional function that takes care of the recursion.  A fixed point
    combinator on the value level can be used to tie the knot.  This way of
    outsourcing the recursion to the caller using a fixed point combinator is
    similar to the trick on the type level.
    }

    \item
    \fancy{
    Functions can be described as algebras or coalgebras and be lifted to true
    cata- and ana- and paramorphisms using specialized folds and unfolds.  This
    technique is very well documented in literature\cite{bananas} and should be
    powerful enough to express all functions on our data
    types\cite{paramorphisms}.
    }

    \item
    \fancy{
    Another option is to use program transformations to convert existing
    recursive function into open variants. While this is possible in theory is
    requires a lot of meta programming to achieve this, e.g. using Template
    Haskell\cite{th}.
    }

  \end{itemize}

  \fancy{
  So, there are several ways to factor out the recursion from the functions
  that operate on the recursive data structures and it is not inherently clear
  which of these is the best. An interesting research topic for this project
  will be to figure out what approach is the most transparent and easy to use.
  }

  \subsection{Annotated fixed points}

  \fancy{
  As illustrated above, when we abstract away from all the recursive positions
  in both our data types and the functions working on our data we can get back
  our original definitions by tying the knot using a fixed point combinator.
  The control over recursion is now shifted from the definition of the data
  structure to the framework. This can be used to generically annotate the
  behaviour.  
  }

  \fancy{
  The fixed point combinator at the data type level can be used to store
  additional information inside the recursive points of our data types. This
  can be done using an annotated fixed point combinator. First we have to
  define composition on type level.
  }

>infixr :.:
>newtype (f :.: g) a = C { unC :: f (g a) }

%if False

>  deriving Show

%endif

  \fancy{
  The annotated fixed point can now be defined like this:
  }

>type AnnFix f ann = Fix (f :.: ann)

  \fancy{
  The fixed point combinator at the value level can be used to perform
  additional actions when the original function would otherwise have gone into
  recursion directly. One way to do this is to write an \emph{open} and
  \emph{lifted} variant of such a function, like the following example of the
  function |count| on our |Tree| data type. The |Query| type synonym is used to
  hide some details.
  }

>type Query g f m c = (f -> m c) -> g f -> m c

>count :: Applicative g => Query (FTree a) f g Int
>count _    FLeaf            = pure 0
>count rec  (FBranch _ l r)  = (\a b -> a + b + 1) <$> rec l <*> rec r

%if False

> -- $

%endif

  \fancy{
  This function operates inside \emph{some} applicative functor and is
  parametrized with an additional function \emph{rec} that takes care of the
  recursion. By using a specialized fixed point combinator we can tie the knot
  and perform an additional task where the function recurses.  For example, we
  can print out the sub structures we are processing:
  }

>makePrintingQuery
>  ::  Show (f (Fix f))
>  =>  Query f (Fix f) IO c
>  ->  Fix f -> IO c
>makePrintingQuery q f  = liftM out (m f) >>= w
>  where  w     = q (m >=> w . out)
>         m a   = print a >> return a

  \fancy{
  With this function we can annotate all open lifted query functions to print
  out all intermediate sub structures. For the |count| function defined above
  this becomes:
  }

>printCount :: Show a => FixTree a -> IO Int
>printCount = makePrintingQuery count

  \fancy{
  This way we can annotate the data structure with new functionality that
  changes the representation of our structures but not the way we write the
  original algorithms.
  }

  \subsection{Persistent data}

  \fancy{
  By annotating the recursive points of the data structure with pointers to
  locations in a file-based storage heap instead of the real sub structures it
  can be made persistent on disk. Every non-recursive part of the structure can
  be handled individually without having the entire structure in memory. By
  annotating the recursive functions that operate on the data structure with
  additional actions that read or write the recursive structures from or to
  disk, you can also project the functions to operate on disk. Because all the
  data structures and functions are written without the explicit recursion, all
  of this happens transparently to the writers of the data structure.
  }

  \subsection{Storage heap}

  \fancy{
  Mimicking what is going on in memory on disk requires a data structure tho
  handle the allocation and deallocation of blocks of persistent storage.  A
  heap just like the one in regular application memory with the ability to
  allocate, free and reuse blocks of data will fit our demands. This heap will
  be stored in a single file on disk and should be a able to grow and shrink on
  demand. To allow the algorithms used on data structures to perform on disk
  with the same asymptotic time and space complexity as in application memory,
  seeking for and reading data from a storage block should work in constant
  time.
  }

  \subsection{Binary serialization}

  \fancy{
  In order to store values of arbitrary data types to disk a tool is needed to
  generically serialize values to and deserialize from binary streams.  Generic
  binary serialization is well described in literature and can be done using
  several generic programming techniques\cite{databinary, derive, emgm, syb,
  multirec, compgen, printparse, clean}.  Ideally users of this framework do
  not have to write their binary serialization code or a generic views on their
  data structures themselves.  Such boilerplate code should be taken care of by
  the generic programming library. The section on related work gives a more
  detailed view of possible techniques.
  }

