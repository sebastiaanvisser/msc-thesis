%include polycode.fmt

\section{Problems and extensions}

The previous sections describe, at a very basic level, what components are
essential to come up with a system for data persistency in Haskell that fulfils
our demands. We need a layer for flexible binary data storage, a layer for
generic serialization and deserialization of Haskell data values, a layer to
tie Haskell container data types to the persistent storage, a basic set of
efficient data structures to manage your data and a nice interface towards the
programmer. The concrete implementation of these layers leaves room to a large
amount of design choices and considerations, some of which are enumerated here.

\begin{itemize}

  \item What kind of storage heap can be used to efficiently allocate blocks of
  data on demand and how can be learn from existing in-memory heaps? Do we need
  automatic garbage collection of unused blocks, or do we exactly know when to
  free blocks by analysing the behaviour of our container algorithms?

  \item Can we improve overall performance by caching or fusing several read or
  write actions in-memory into one single action that shares common disk
  accesses? Can this fusion be expressed implicitly, without adapting our
  interfaces.

  \item The system described in this document allows for an aspect oriented way
  of programming by generically annotating data structure and algorithms
  working on them. Can be capture different aspects in the type signature of
  the final system? It would be nice if we could eventually write something
  like this:

> type MyDB a b = Persistent (Caching (FTree a b))

  \item What happens when we try to store infinite data structures that
  properly share sub-structures? Can we detect cycles and threat them as such?
  How does this influences garbage collection? Can we extend the system, maybe
  using compiler extensions, to store not only values of data types, but
  arbitrary computations?

  \item What container data structures can we write to efficiently store and
  lookup records? Can we learn from existing RDBMs to create some fast general
  purpose containers? Can we even use lift existing container data structures
  to annotated forms. For example, by simulating partial actions on partial
  structures in memory and analysing their behaviour?

\end{itemize}

The goal of this project is to come up with a working system for data
persistency in Haskell and to investigate the questions above. How can we learn
from existing techniques, maybe even from other programming languages, what
problems we may encounter and how we might solve them.

