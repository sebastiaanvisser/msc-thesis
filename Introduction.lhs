%include polycode.fmt
%include thesis.fmt

\begin{chapter}{Introduction}

Management of long-lived data is an essential ingredient of a large amount of
modern computer programs. Over the past decades of software technology, plenty
of systems have been developed to store information outside the process space of
a running application. Examples of such systems are structured file systems,
relational databases management systems\cite{rdbms} (RDBMSs), XML document
stores, and key/value databases. All of these tools allow for some
form of structuring the data. For example, file systems allow for a
hierarchical view on data using directories and files, relational database
management systems use a tabular layout in which data can be structured using
rows and columns, and key/value stores build up finite mappings. Giving
structure to the data greatly helps efficiently manipulating the data. Unfortunately,
the data structures used by such systems do not always match the data
structures used inside the computer program that uses the data. The result of
this structure mismatch is that for many computer programs code has to be
written to convert the structure of data when writing it to a long-lived
storage and when reading it back again.

Over the years many solutions have been proposed for this problem, for many
different contexts. For example, for most object-oriented (OO) programming
languages there exists Object-Relational Mappers\cite{orm} (ORMs) that allow
for a transparent mapping between objects and tables within a relational
databases.  ORM tools exploit the structural properties of both the table based
RDBMSs and objects in an object-oriented language to derive an automated
mapping to the database. Automating the conversion step can save time
in the development process.

Unfortunately, an object-relational mapping is not applicable to functional
programming languages like Haskell. Haskell uses algebraic datatypes to
structure data, not objects described by classes.  There are currently several
ways of making algebraic datatypes in Haskell persistent on an external storage
devices, but unfortunately all of these methods have some limitations. We 
now discuss three commonly used techniques for data persistence in Haskell and
explain the limitations of these approaches.

\begin{itemize}

\item \textbf{Relational databases.}
There are several packages available for Haskell that use connections to
existing relational database management systems to store values outside of
application memory. Either using manually written conversions functions or via
a generically derived conversion, values of arbitrary types are mapped to
database rows.  In the related work section \ref{sec:relrdbms} some examples
can be found of database connectors for Haskell.

Unfortunately this mapping from algebraic datatypes to the table based layout
of RDBMSs, without losing any structural information, tends to be rather hard
and inefficient. Because the mapping from complex hierarchical Haskell data
structures to a table based database system is hard, most database connectors
only support a restricted set of types to be marshalled. This observation leads
to the conclusion that writing a Haskell data model that describes the
structure of an existing relational database is often easy, but
\emph{using a relational database to store the values of the data model of an
existing Haskell program can be rather hard.}

\item \textbf{Key/value storage.} Similar to the connectors with relational
databases are the mappers to key/value based storage systems. These systems
provide an interface similar to the Haskell |Map| datatype, a finite mapping
from keys to values. This interface can be useful when managing a large
amount of records that can be easily identified by a single key. Lookup of
records by key can often be done efficiently. Example of such key/value
stores can be found in the related work section \ref{sec:relkeyval}.

While key/value stores can be useful, and are often easy to use, they have a
limitation. \emph{Key/Value stores only allow one data structure to be used, a
finite mapping between keys and value.} 

\item \textbf{Textual and binary serialization.}
Another possibility to store values of arbitrary datatypes outside the
application memory is to serialize the entire value to a textual or binary
representation. This representation can be written to and read from disk at
once.

The Haskell |Show| and |Read| type classes are primitive examples of such a
tool. These classes are used to print and parse textual representation of
Haskell values and can be derived automatically by the compiler for most datatypes.
More advanced tools exist that use binary serialization to apply the same
technique more space and time efficient. Some of the libraries are fast
and make use the types of values to prevent creating to much administrative
overhead when saving the binary data.

\emph{The big disadvantage of these libraries is that values can only be
written and read at once.} Only single values can be serialized to a textual or binary form. These libraries do not add any structure to the data, no partial structures can be processed without deserializing the entire structure.
Due to this property, these techniques do not scale 
well when dealing with large amounts of data. 

\end{itemize}

\begin{section}{Contributions}

We have seen that there are several ways in Haskell to save the application data on a
persistent storage like a hard disk. Unfortunately, all of these approaches
have limitations. In this document we describe the design and implementation fo
a new framework for data persistency in Haskell.  The storage framework this
document describes has the following properties.

\begin{itemize}

\item \textbf{Pure Haskell} The framework is written entirely in Haskell. No
connections to existing database tools are used. Writing the framework in pure
Haskell has some important advantages. First of all the system remains
lightweight and easily deployable in an existing Haskell environment. No
foreign function interfaces to existing c-libraries are needed. Second, by
writing the entire system in Haskell we are allowed to exploit the full power
of the type system to guide our implementation. We use the Haskell type system 
to both make sure our implementation is correct and helps us designing an
interface to the end-users. Pure Haskell does not mean all of the code is
purely functional, for the low level storage code we use the |IO| monad to read
and write to disk. Although we try to minimize the amount of code that performs
side effects we can never get rid of it entirely.

\item \textbf{File based.} The system uses a technique for binary serialization
to convert Haskell values to a stream of bytes. This stream of bytes is stored
in a file based storage heap on disk. The file based heap can contain multiple
blocks of binary data and can grow and shrink on demand. When the program
terminates the data is still available at disk for any consecutive runs.

\item \textbf{Generic.}
The framework can be used to store values of a large set of Haskell datatypes. 
For all the datatypes that can be given an instance for the |Binary| type class values can be stored on disk. All recursive datatypes that are explicitly parametrized with the recursive positions can be stored and accessed incrementally.
Generic
programming techniques are used to automatically derive code for binary
serialization and deserialization of most Haskell datatypes.

\item \textbf{Incremental.}
The storage framework allows incremental access to the persistent data stored
in the file based heap. Operations on a persistent data set -- like updates,
insertions and queries -- can be performed without reading the entire data
store into memory. Most Haskell container data structures are
represented by recursive datatypes. Examples of such data structures are lists, binary trees, finger trees, and Patricia tries.

The framework stores all non-recursive
nodes of a recursive datatype in a separate block on disk and connects
individual nodes together using pointers. By slicing larger data structures in
pieces we can make sure fast incremental access to the data is possible. The
amount of read and write actions are minimized as much as possible, because
disk access is significantly slower compared to memory access. Incremental
access is an important feature for writing programs that scale well to large
data sets.

In order to slice up recursive datatypes the operations on these types have to abstract away from recursion.
Abstracting away from recursion when writing operations might be a bit more difficult than writing plain recursive operations.

\item \textbf{Mutable.}
The persistent data structures on disk are used as mutable data. This means
that all update operations modify the existing data structure in-place and no
sharing happens. By using a mutable data structure the system acts like a
database which makes the behaviour of the system predictable.

For some applications immutable data structures are a better fit than mutable data structures. Immutability is done quite efficiently by most compilers for functional language by sharing parts of data structures between different uses.
The possible extension of the system to allow sharing and
immutability are discussed in the chapters on related work and future work.
Unfortunately the current project does not allow sharing and immutability.

\item \textbf{Layered.}
The framework can conceptually be separated in three different layers of
functionality.
  \begin{enumerate}

  \item The lowest layer is the \emph{persistence layer} that allows values of
  Haskell datatypes to be stored on disk. The persistence layer ensures
  recursive datatypes are sliced into pieces and stored in separate blocks on
  the storage heap.

  \item The second layer is the \emph{data layer} and contains the persistent
  versions of the recursive container data structures. The recursive data
  structures are written in a special way that guarantees the \emph{persistent
  layer} is able to lift them to the storage layer automatically.

  \item The third and top-most layer is the \emph{user layer}. This layer is
  the interface to the developer of the application requiring a persistent data
  store. The developer can choose a data structure that fits her needs and use
  high level operations to manipulate the data.

  \end{enumerate}

\item \textbf{Transparent.}
The framework is transparent to both the developer and the designer of the data
structures that are made persistent. 

When writing a persistent version of a data structure, no knowledge of the
inner workings of the persistence framework is needed. The recursive datatypes
and operations on the datatypes are written in pure Haskell code and are
agnostic of the persistence layer.
Only specific kinds of operations can be lifted to the persistent store: operations that abstract away from recursion. When operations are written as algebras for certain morphisms on recursive datatypes,
they cab automatically be lifted to work on the block based
storage heap instead of working in application memory.

Manipulation of the persistent data structures by the application developer is
just like manipulating a regular in-memory data structure. When performed
operations on the data structure all I/O actions to the file heap are
abstracted away. The one big difference is that all operations are
lifted to a \emph{monadic} computational context. The monadic context allows
the framework to transparently convert the operations to work on the heap, but forces the users to compose functions using monadic combinators instead of regular function composition.

\end{itemize}

\end{section}

\begin{section}{The source code}

This document describes the design and implementation of a generic storage
framework for the functional programming language Haskell. The framework is
implemented as a Haskell library and which is called
\emph{Islay}\footnote{After the Scottish island of Islay, the source of some
very fine whiskys.}. The source code of the library is available online on:

\begin{center}\url{http://github.com/sebastiaanvisser/islay}\end{center}

Additionally, the sources of this document can be found:

\begin{center}\url{http://github.com/sebastiaanvisser/msc-thesis}\end{center}

At the moment of writing the library is only a prototype and not in
release-worthy state. When the library matures it will eventually be released
on the public Haskell packages storage \emph{Hackage}\footnote{http://hackage.haskell.org/packages/hackage.html}.
This documentation is a
description of the ideas and techniques from this library, not of the actual
implementation. This means that certain parts of the code and examples may not
correspond to the library exactly.

\end{section}

\begin{section}{Motivating example: spatial indexing}

The advantage of relational databases is the generality when it comes to
storing data. Using (or misusing) the generic table based layout of an RDBMS
almost always ensures that your application data can be saved in \emph{some}
structure.  Unfortunately it is not always easy to perform fast queries over
data when the structure of information in the table based layout does not fit
the algebraic layout of the original algebraic datatype well.

To illustrate this disadvantage, consider an example application that stores a
mapping from two-dimensional geometrical coordinates to business relations.
Storing such a mapping in a database is simple and does not take a 
complicated structure. The problem arises when you want to perform efficient
spatial queries over the data, like getting the |n| nearest business relations
to the city center of Utrecht. Efficient SQL queries that perform such specific
tasks are hard to write. Because spatial indexing is a common problem the
are many solutions available that are built into existing databases. For
example all of the Oracle, Microsoft SQL Server, MySQL, and PostgreSQL database
systems have custom support for custom support for spatial queries. Because the
internal data structures of these RDBMSs were not optimized for spatial
queries, support for spatial types have been built as a database extension.

There are several data structures capable of performing
such spatial queries efficiently.  A quadtree is a domain specific data
structure specialized for efficient indexing of spatial information.  Quadtrees
are specializations of multidimensional search trees\cite{multitree}.  Elements
inside a quadtree are saved and indexed based on their geometrical coordinates
which results in efficient spatial lookups.  Finding the |k| elements
nearest to a specific elements using a quadtree can be done in not more than
$O(k$ log $n)$ time.  Performing the same task with the same asymptotic
running time using SQL queries on an table based RDBMS is difficult and
this is the reason database developers created built-in support for spatial
queries.

Storing data is something most databases do well, performing efficient
domain specific queries over a collection of elements is best done
using specialized data structures.  Storing a large collection of data outside
application memory and still being able to perform specialized queries over the
data is still an unsolved problem in most programming languages and
accompanying storage libraries.

The framework described in this document tries to solve this problem by
allowing application developers to use their own domain specific data
structures as a persistent storage. For example, a developer can implement a
quad-tree in Haskell and store values on disk using this quad-tree. 
By projecting the internal
structure of container datatypes to an isomorphic variant on disk, specialized
algorithms are still applicable with the same time and space complexities.  The
framework enables developers of container libraries to focus on the internal
structure and allows for writing efficient algorithms without worrying about
the internals of the storage layer.

\end{section}

\begin{section}{Overview}

This section gives a brief overview of the next chapters.

In chapter \ref{chap:fixpoints} we explain how to use a fixed point combinator
to obtain control over the recursive positions of recursive data structures. By
parametrizing recursive datatypes with a type parameter that is used at the
recursive positions, the users of the datatype are allowed to change the values
stored at these positions. The fixed point combinator takes a datatype with a
type parameter for the recursion and instantiates this parameter with its own
fixed point, making the datatype truly recursive again. By replacing the
regular fixed point combinator with an annotated fixed point combinator, we are
able to store additional information in the recursive positions of a 
datatype. As an example, in section \ref{sec:fixann} we show how to use an
annotated fixed point combinator to build an annotated binary tree datatype. We
show that using an identity annotation at the recursive positions gives us back
a structure isomorphic to a regular binary datatype. In section
\ref{sec:annfun} we introduce two Haskell type classes that can be used to wrap
and unwrap constructors of recursive datatypes with an annotation variable.
These type classes allow us to associate functionality with the annotation
variables stored in the recursive positions. 

Working with annotated recursive data structures on itself is not easy. All the
operations working on the datatypes need to wrap and unwrap the annotations
every time a recursive positions is touched. This makes the operations hard to write.
In chapter \ref{chap:morphisms} we
solve this problem by writing operations that abstract away from recursion. We
show how to write operations on recursive data structures as algebras that abstract away from recursion.
The algebras can be interpreted by generic traversal functions.
In section \ref{sec:para} and \ref{sec:apomorphisms} we define both an
annotated \emph{paramorphism} and \emph{apomorphism} function. Both the paramorphism and
apomorphism are specific forms of recursive functions that either destruct or
construct recursive datatypes. Both morphisms use an algebraic description of a
recursive operations. However, the algebraic descriptions do not go into
recursion themselves, this is done by the morphisms function.

The paramorphism uses an algebra to destruct an annotated data structure to
some result value, thereby using the annotation type classes to unwrap the
recursive values out of the annotations. The apomorphism uses a coalgebra to
produce an annotated data structure from some seed value, thereby using the
annotation type classes to wrap the recursive values in new annotations.
In section \ref{sec:endopara} and
\ref{sec:endoapo} we define \emph{endomorphic} variants on the paramorphisms
and the apomorphisms. Endomorphisms allow us to modify -- not create or destruct -- existing recursive data
structures.

In section \ref{sec:paraapp} we show how to compose multiple algebras into one
using the Haskell |Applicative| type class.  Composing algebras can simplify
building algebraic operations.

The traversals described in this chapter all work on annotated structures. When
the wrap and unwrap functions associated with the annotation requires a strict
computational context, like |IO|, the running time of the operations can be
negatively influenced. In section \ref{sec:laziness} we solve the strictness
problem by enforcing the traversals to be as lazy as possible.

In chapter \ref{chap:heap} we introduce a file based heap that forms the low
level storage layer of the framework. Section \ref{sec:heaplayout} describes
the block based layout of the heap and how we can use offset pointers to refer
to blocks of data. We describe the basic operations of the heap: reading data
from a block, writing data to a block, allocation a new block, and freeing an
existing block. In section \ref{sec:rootnode} we build three functions that
help use to perform heap operations that require a fixed root node as a
starting point. In \ref{sec:runheap} we show how to run heap operations against
a binary file. All heap operations work inside a heap context, a monadic
computational context that abstracts all low level I/O operations.

In chapter \ref{chap:storage} we show how we can make the pointer type from the
storage heap from chapter \ref{chap:heap} an instance of the annotation type
classes from chapter \ref{chap:fixpoints}. Wrapping a value in a pointer annotation
represents writing a single node of a recursive data structure to the heap.
Unwrapping a value out of a pointer annotation represents reading a value from the heap. In section
\ref{sec:persistenttree} we show how the pointer annotation can be used to
derive a persistent binary tree. We specialize the generic binary tree
operations to work with the pointer annotation in the heap context. Using these
operations in the heap context results in a binary that lives on disk instead
of in application memory.

The storage framework from chapter \ref{chap:storage} can be used to build
persistent variant of recursive data structures. Because the recursive data
structures now live on disk, they survive the lifetime of a single program
invocation. Unfortunately this framework only works for regular recursive
datatypes. There is a class of recursive Haskell datatype that can not be used
by this framework. In chapter \ref{chap:indexed} we extend the framework to
allow making indexed recursive data types persistent on disk. We extend the
annotation framework and the generic traversals to work higher order datatypes.
In section \ref{sec:fingertree} we introduce the finger tree data structure. We
write down the definition of the finger tree as an indexed generalized
algebraic datatype. We show how to write operations on the finger tree as
higher order algebras. 

In chapter \ref{chap:relatedwork} we give a listing of work related to this
project. We show existing libraries for data storage in Haskell and explore
some topics that influence this work or can be an inspiration for future work.
In chapter \ref{chap:futurework} we give a list of possible extensions of this
framework. Lots of functionality can be added and lots of design decisions can
be reconsidered. This chapter states the most obvious of topics for possible
future work. In chapter \ref{chap:conclusion} we wrap up with a conclusion.

\end{section}

\end{chapter}

