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
Unfortunately values of datatypes in Haskell can only be manipulated in memory.
When data structures grow too large to fit in application memory or when the
data outlives the running time of a single process there is no convenient way
to store data structures outside application memory.

For most object-oriented (OO) programming languages there exists
Object-Relational Mappers \cite{orm} (ORMs) that allow for a transparent
mapping between objects and tables within a relational databases. Automating
derivation of database queries from the structure of objects can save time in
the development process.

Many attempts have been made to map values of algebraic datatypes to 
relational databases tables. Due to the mismatch between the column based
layout of relation databases and the structure of functional data structures
only limited solutions exist.

We identify three important properties of a functional persistence
framework:

\begin{enumerate}
\item \textbf{Efficiency:}   Incremental access to parts of the data.
\item \textbf{Flexibility:}  The system should provide ways to store domain specific data structures.
\item \textbf{Transparency:} Users should not be bothered with the details of the underlying system.
\end{enumerate}

In this paper we present a framework for saving functional data structures in
Haskell to a database file on disk. The data structures are stored in a way
that allows partial access to the structure, which allows us to efficiently
manipulate the data. The storage framework is not restricted to a single data
structure. Using generic programming we are able to lift a large class of
recursive datatypes to work on a persistent storage.

Consider the following two simple Haskell program:

> build :: IO ()
> build = run "squares.db" $
>   do  let squares = [(1,1),(3,3),(4,16),(7,49)]
>       produce (fromListP squares)

> find :: IO ()
> find = run "squares.db" $ forever $
>   do  num  <- liftIO (read `liftM` getLine)
>       res  <- query (lookupP num)
>       case res of
>         Just sqr  -> liftIO (print (num, sqr))
>         Nothing   -> modify (insertP num (num * num))

Even without knowing any details about the code, by looking at it line by line
the programs are easy to grasp. The first little program opens up database file
on disk called \texttt{squares.db} and uses it to store a small mapping from
numbers to their squares. The second program, that can run independently from
the first program, opens up the same database and starts to a loop that
interacts with the user. Every iteration the program reads a number from disk
and tries to look up the square of that number in the database. When a square
is found this will be reported back to the user, when no square is found is
computed and added to the database.

The operations that are run against the database file run in their own monadic
context that supports interweaving arbitrary |IO| actions using |liftIO|. In
the code we three operations that manipulate the actual database file:
|produce|, |query| and |modify|. The three functions are used to lift a special
kind of operations on functional data structures to work on a database file on
disk. In the example program we see a |fromListP|, |lookupP| and an |insertP|
function, which are operations that manipulate a persistent map from keys to
values implemented as a binary search tree, similar to Haskell's
\texttt{Data.Map} \cite{bintree}.

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














