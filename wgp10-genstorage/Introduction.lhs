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
in Haskell to a database file on disk. We do not use relational databases, but
build or own system that matches the structure of algebraic datatypes.

We identify the three important properties of our framework:

\begin{enumerate}
\item \textbf{Flexibility:} The storage system doest not impose a single way to
structure the data. Both general purpose and domain specific data structure can
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
illustrates the use of our storage framework:

> build :: IO ()
> build = run "squares.db" $
>   do  let squares = [(1,1),(3,3),(4,16),(7,49)]
>       produce (fromListP squares)
>
> find :: IO ()
> find = run "squares.db" $ forever $
>   do  num  <- liftIO (read `liftM` getLine)
>       res  <- query (lookupP num)
>       case res of
>         Just sqr  -> liftIO (print (num, sqr))
>         Nothing   -> modify (insertP num (num * num))

The first little program opens up database file
on disk called \texttt{squares.db} and uses it to store a small mapping from
numbers to their squares. The second program, that can run independently from
the first program, opens up the same database and starts to a loop that
interacts with the user. Every iteration the program reads a number from disk
and tries to look up the square of that number in the database. When a square
is found this will be reported back to the user, when no square is found is
computed and added to the database.

The operations that are run against the database file run in their own monadic
context, allowing to sequence multiple actions in one database action. We offer
three basic operations to manipulate the database file: |produce|, |query| and
|modify|. The three functions are used to lift operations on persistent
functional data structures to work on the database file.  In the example
program we lift |fromListP|, |lookupP| and the |insertP| function to manipulate
a persistent mappign from keys to values implemented as a binary search tree,
similar to Haskell's \texttt{Data.Map} \cite{bintree}.

In section \ref{sec:fixpoints} we implement a generic framework for working
with annotated recursive datatypes. By parametrizing datatypes with an
additional type variable for the recursive positions we gain control over the
recursion. We store annotations at the recursive positions and associate
functionality with the construction and destruction of recursive datatypes.
In section \ref{sec:patterns} we write operations by abstracting away from
recursion using recursion patterns. Using \emph{catamorphisms} and
\emph{anamorphisms} we lift annotation agnostic algebras to operate on
annotated datatypes.
In section \ref{sec:heap} we implement an on-disk heap data structure for block
based binary storage. This heap allows dynamic allocation and freenig of blocks
of binary data on disk. The structure can grow and shrink on-demand.
In section \ref{sec:storage} we use pointers as offsets to blocks on the
storage heap as annotations for the recursive positions of datatypes. Using
pointers as annotations yields data structures that are stored on disk.  As a
running example we show how to apply the storage framework to create a
persistent binary tree.














