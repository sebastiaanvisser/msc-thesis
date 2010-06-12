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













\begin{itemize}

\item - Algebraic datatypes in Haskell provide a powerful way to structure data. \\
      - Recursive datatypes can be used to create functional data structures.
\item - Unfortunately there are no good way to store functional data structures on disk. \\
      - We identify three important properties of a functional persistence framework:
      \begin{enumerate}
      \item Efficiency:   Incremental access to parts of the data.
      \item Flexibility:  The system should provide ways to store domain specific data structures.
      \item Transparency: Users should not be bothered with the details and limitations of the underlying system.
      \end{enumerate}

\item Current persistence solutions in Haskell do not provide these three properties:
      \begin{enumerate}
      \item Connections to existing relational database management systems: \\
            - These system allows incremental access, but restrict the users to a table based layout. \\
            - The relational data model is forced upon the user.
      \item Packages for binary serialization like Data.Binary
      \end{enumerate}

\item State your contributions:
      \begin{enumerate}
      \item - We implement a generic framework for working with annotated recursive datatypes. \\
            - The system allows to annotate both data and functionality for all recursive datatype in Haskell.
      \item - We implement a on-disk heap data structure for block based binary storage. \\
            - This heap allows for generic on-demand growing and shrinking binary data storage on disk.
      \item - We use both the generic annotation framework and the heap to derive a generic storage framework. \\
            - The system allow to store arbitrary Haskell data types on disk. \\
            - Recursive datatypes can be accessed incrementally.
      \item - As an example we show how to apply the framework to create a persistent binary tree. \\
            - We indicate the system can even be used for indexed datatypes.
      \end{enumerate}

\end{itemize}

