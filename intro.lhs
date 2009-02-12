%include polycode.fmt

\section{Introduction}

\review{Haskell is a powerful language for processing data. Algebraic data
types (ADTs) allow programmers to compose their data in a structured and type
safe way, functions can be used to process this data. All of this takes place
in internal memory, somewhere deeply buried in the application's storage heap.
Unfortunately there are currently no systems available for Haskell that allow
the programmer to process data stored on an external storage device with the
same power and flexibility as you would have when dealing with in-memory data.}

\review{Currently there are several ways to make Haskell data persistent on
disk.  Unfortunately, all of the systems available lack some important
properties or do not fit the functional paradigm well. The following
enumeration lists the most common ways of persisting your data in Haskell
today.}

\begin{itemize}

  \item
  \review{There are good connectors to existing relational database management
  systems (RDBMS) available. These connectors allow you to directly query an
  RDBMS using the SQL query language. This gives developers the complete power
  of databases, like fast querying, indexing, transactions etc.}

  \review{The problem with these systems is that is does not fit the functional
  world of algebraic data type very well. Making use of such a system implies
  you have to map all your data from and to database tables. Clever tricks must
  be used to map the structure of your ADT to database tables efficiently.}

  \review{\emph{Example Haskell packages for connecting to database management
  systems: HDBC\cite{hdbc}, sqlite\cite{sqlite}.}}

  \item
  \review{Others libraries build on top of these RDBMS bindings and allow
  marshalling of Haskell data generically. Using generic programming techniques
  they allow mapping values of any \footnote{Most libraries do have some
  limitation on what ADTs can be processed, for example only regular data
  types.} data type to database rows and vice versa.}

  \review{While these system really add some power, because developers do not
  have to write the marshalling code manually, these system are limited in
  their own way. Because RDBMSs are used as a back-end it is hard to map
  efficient domain specific functional data structures to the database world.
  Only real data records can be made persistent, the container structures in
  which you would normally manage your data are replaced by the internals of
  the database.}

  \review{\emph{Currently both Americo Vargas Villazon and Chris Eidhof are
  working on (or making use of) generic bindings to existing database
  systems.}}

  \item
  \review{Not all libraries for data persistency make use of RDBMSs, other
  libraries directly serialize Haskell data types to blocks of textual data or
  binary data. These blocks can be saved to- or read from file at once in a
  type safe way. This approach can be rather lightweight, because it does not
  rely on an external systems and can be implemented in pure Haskell.}

  \review{Everyone working with Haskell knows how to use the |Read| and |Show|
  classes to serialize and deserialize data to a textual form. This system is
  really useful for small projects, but is rather slow, does not scale well and
  uses far more space than is necessary.}

  \review{Newer systems allow serialization to binary form, which is far more
  space efficient than a textual representation and can be deserialized much
  faster.  The major disadvantage of such systems is that only monolithic
  blocks of data can be stored to- or read from disk at once. These libraries
  do not allow partial updates of data and do not allow selectively querying
  for specific records. This can be big problem when dealing with large data
  sets.}

  \review{\emph{Examples of package using generic techniques to serialize
  binary data: binary\cite{databinary}, happs-state\cite{happsstate}.}}

\end{itemize}

\review{This document proposes a new lightweight approach to data persistency
in Haskell which does not compromise the functional paradigm. Inspired by the
in-memory representation of Haskell data types this systems will generically
lift functional data structures to work on a persistent storage.}

\review{The project is quite ambitious in the sense that there are more than a
handful features and optimizations possible that are not essential to get an
initial system working. The initial goal of this project is to have at least the
following properties.}

\begin{itemize}

  \item
  \review{The system should be able to store values of arbitrary Haskell types
  generically. This means the programmer does not have to write their own
  marshalling code.}

  \item
  \review{The storage can be structured using functional data structures as are
  commonly used for maintaining and manipulating data in-memory in most Haskell
  programs. This would imply that existing algorithms can be uniformly lifted
  to work on persistent data.  Developers should be able to store their make
  there data persistent at disk still in their favourite domain specific
  container type, like finger trees, trie based maps, geometric quad trees,
  lists, etc.}

  \item
  \review{The implementation of container data structures specialized for the
  persistent machinery should be as easy as possible. Developers should not be
  bothered by the inner workings of the system. For example. both developers
  and users should not be bothered with |IO| actions when dealing with pure
  functions on pure data.}

  \item
  \review{It should be possible to freely navigate the persistent data structure
  without reading and writing the entire storage at once. For example,
  performing a find action on an persistent AVL tree should actually run in
  |O(log n)| and should not touch more data than necessary for this action.}

  \item
  \review{The system should be implemented in pure Haskell and should not rely
  on external database tools. This keeps the system lightweight and gives us
  the possibility to keep things `functional'}

\end{itemize}

\review{The next sections explains the technical aspects that are needed in
order to compose the system. Because this document is still a proposal, some
functions are not yet implemented as displayed here and might need some
additional research.}

