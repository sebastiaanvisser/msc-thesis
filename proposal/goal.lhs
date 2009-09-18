%include polycode.fmt

\section{Goals}

For this thesis project to succeed at the least the following topics should
have been researched and implemented as part of the framework.

\begin{itemize}

\item A file-based storage heap.
\item A framework to lift data structures to persistent variants.
\item One general purpose data structure.
\item One domain specific data structure.
\item A garbage collector.
\item A transactional memory cache.
\item An example application and database.

\end{itemize}

\subsection{Heap}

The storage heap will be the back-end of the persistence framework and will
manage all low level IO operations.  We at least need one sufficiently
efficient heap implementation with the interface described in section 3.6.

\subsection{Persistence framework}

We need a framework that helps developers of custom domain specific container
data structures to lift their operations into a context for persistent storage
(the |Storage| monad as described in section 3.6).

The primary goals is to have at least the ability to generically lift morphism
based functions to persistent operations.  Lifting algebraically defined
operations would most certainly imply using some generic programming library to
derive monadic variants of catamorphisms, anamorphisms and paramorphims and
accompanying algebras and coalgebras.

Because the one of the purposes of this project is to investigate the possible
ways of writing recursive agnostic functions of inductively defined data types,
the framework should not rely on a single way of lifting data structures and
algorithms.  This part of the project is somewhat open and needs some
additional research.

\subsection{General purpose data structure}

We should at least have one sufficiently efficient general purpose data
structure.  A simple choice for such a data structure would be a dictionary
based on a balanced binary tree similar to the Haskell |Data.Map|
implementation.

Another interesting general purpose data structure with a nice functional
structure and good efficiency is the finger tree\cite{fingertree}.  Finger
trees are the based of the Haskell |Data.Sequence| library.

\subsection{Domain specific data structure}

To prove the real power of the system it is desirable to have at least one
special purpose data structure included.  Some domain specific data structures
are very hard to map to relational database management system, while this
framework should be able to handle them well.

An example of such a domain specific data structure could be a space partition
tree, like quadtrees or octtrees.  These can be used to efficiently store and
retrieve geometric data.  Another interesting example data structure would be a
persistent Patrica trie that can be used for very fast indexing of textual
data.

\subsection{Garbage collector}

Sharing in Haskell creates the possibility that certain parts are included in
the same data structure more than once.  Sharing has the implication that the
modification of persistent data does not allow us to unconditionally throw away
or overwrite the original value.  These values might be shared among other
parts of the structure.  To make sure that the storage heap does not grow with
every modification we are in need of a garbage collector to clean up unused
blocks of data. 

We need at least one, possibly conservative, garbage collector to show that the
system is able to allow some kind of sharing.  The simplest and fastest garbage
collector model that can be implemented is probably a reference counting
garbage collector.  Reference counting garbage collectors are infamous because
of their inability to detect and cleanup cycles. 

By initially only allowing the persistence of non-cyclic structures with
possible sharing we can at least show that the framework does have support for
sharing.  In a later stage it might be desirable to make a less conservative
garbage collector that can also cleanup cyclic structures.

\subsection{Transactional cache}

To allow safe and concurrent access to the underlying storage layer we need to
implement or use some form of transactional cache.  A transactional cache is a
single entry point that runs operations on the persistent data structure using
an in-memory transaction.  Using a transactional cache all operations can be
performed in-memory and can atomically be committed to the underlying storage
heap.

Adding an in-memory cache layer can be used for both transactional access to
the persistent data structure, but can also be used as a fast-access in-memory
view on the data.  In-memory operations are far more efficient than true disk
accesses.  Frequently and recently used blocks of data can be cached in-memory
to allow speedups of otherwise IO intensive operations.

\subsection{Example application}

To illustrate the use of the framework and to be forced to think about the API
exposed to the user at least one demo application should be build.  This
application should illustrate the use of the underlying framework by storing a
single data structure containing a reasonable large collection of records.  All
common operations should be included in the demo, like creating a storage from
an existing list of records, inserting new record, querying records by some
index, removing records and counting the number of records.

A demo application can also be used for testing and benchmarking the
performance of the framework.  Creating a set of test cases for the framework
and profiling these in an automated way can be very helpful when trying to
improve performance.  And, naturally, building a sample application helps to
pinpoint possible problems which helps when steering the direction of the
thesis.

