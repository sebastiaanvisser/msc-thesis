%if False

> {-# LANGUAGE
>     ScopedTypeVariables
>   , FlexibleInstances
>   , FlexibleContexts
>   , MultiParamTypeClasses
>   , UndecidableInstances
>   , GeneralizedNewtypeDeriving
>   #-}
> module Introduction where

> import Control.Monad
> import Control.Monad.Trans
> import Heap hiding (read)
> import Prelude hiding (lookup)
> import Storage

%endif

\section{Introduction}

Algebraic datatypes in Haskell provide a powerful way to structure data.
Recursive datatypes can be used to create functional data structures.
Unfortunately, when data structures grow too large to fit in application memory
or when the data outlives the running time of a single process there is no
convenient way to store data structures outside application memory.

For most object-oriented programming languages there exist Object-Relational
Mappers \cite{orm} that allow for a transparent mapping between objects and
tables within relational databases. Automated derivation of database queries
from the structure of objects can save time in the development process.
Many attempts have been made to map values of algebraic datatypes in Haskell to
relational databases tables. Due to the mismatch between the column based
layout of relational databases and the structure of functional data structures
only values of specific types can be marshalled.

In this paper we present a new framework for saving functional data structures
in Haskell to a database file on disk. We do not use relational databases. Instead,
we build our own system that relies on the structure of algebraic datatypes. We
expose datatypes as fixed points of functors, and introduce the concept of
effectful annotations. By writing operations as instances of specific recursion
patterns such as catamorphisms and anamorphisms, we then obtain data structures
that can be used in various ways. In particular, they can be used both in memory
and on disk.

We identify the three important properties of our framework:
\begin{enumerate}
\item \textbf{Flexibility:} The storage system does not impose a single way to
structure the data. Both general purpose and domain-specific data structures can
be stored on disk.
\item \textbf{Efficiency:} By enabling incremental access to parts of the data
we allow efficient manipulation of large collections of data.  Algorithms
working on a persistent data structure have the same asymptotic running time as
their in-memory counterpart.
\item \textbf{Transparency:} The final interface to the users uses common
Haskell idioms. Users are not be bothered with the inner workings of the
storage system when manipulating persistent data structures.
\end{enumerate}

Consider the following two simple Haskell programs that on a high level
illustrate the use of our storage framework:

> build :: IO ()
> build = run "squares.db" $
>   do  let squares = [(1,1),(3,3),(4,16),(7,49)]
>       produce (fromListP squares)
>
> find :: IO ()
> find = run "squares.db" $ forever $
>   do  num  <- liftIO (read `liftM` getLine)
>       res  <- consume (lookupP num)
>       case res of
>         Just sqr  -> liftIO (print (num, sqr))
>         Nothing   -> modify (insertP num (num * num))

The first program opens a database file
called \texttt{squares.db} and uses it to store a mapping from
numbers to their squares. The second program -- that can run independently from
the first -- opens up the same database and starts an interactive loop.
In every iteration, the program expects a number
and tries to look up the square of that number in the database. If a square
is found, it will be reported back to the user; when no square is found, the
square is computed and then added to the database.

The operations that are run against the database file run in their own monadic
context, allowing to sequence multiple actions in one database action. We offer
three basic operations to manipulate the database file: |produce|, |consume| and
|modify|. The three functions are used to lift operations on persistent
functional data structures to work on the database file.  In the example
program, we lift |fromListP|, |lookupP| and |insertP| to manipulate
a persistent mapping from keys to values implemented as a binary search tree,
similar to Haskell's @Data.Map@ library~\cite{bintree}.

In Section~\ref{sec:fixpoints}, we repeat the basic idea of expressing
datatypes as fixed points of their pattern functors, and defining
functions by instantiating recursion patterns such as catamorphisms
and anamorphisms. We then show how to
add annotations to the recursive positions of datatypes (Section~\ref{sec:annotations})
and how to associate functionality with the creation and removal of annotations.
In Section~\ref{sec:patterns}, we discuss how annotations affect recursion
patterns and functions that are defined in terms of these patterns. We show that,
in many cases, we can easily lift algebras written in a pure, annotation-agnostic
style to work on annotated datatypes.

In order to define an annotation suitable for our generic storage framework,
we need an on-disk heap structure that can hold blocks of binary data. Such
a heap is discussed in Section~\ref{sec:heap}. It allows dynamic allocation
and feeing of blocks on disk, and can grow and shrink on demand. We then
use pointers as offsets to blocks on the
storage heap as annotations for the recursive positions of datatypes (Section~\ref{sec:storage}),
yielding data structures than can be stored on disk. As a
running example, we show how to apply the storage framework to create a
persistent binary tree.

We discuss some subtleties of our approach as well as opportunities for
future work in Section~\ref{sec:discussion}, present related work
in Section~\ref{sec:relatedwork} and conclude in Section~\ref{sec:conclusion}.

