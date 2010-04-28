%include polycode.fmt
%include thesis.fmt

\begin{chapter}{Introduction}

Management of long-lived data is an essential ingredient of a large amount of
modern computer programs. Over the past decades of software technology plenty
of tools have been developed to store information outside the process space of
a running application. Examples of such systems are structured file systems,
relational databases management systems\cite{rdbms} (RDBMSs), XML document stores,
key/value databases, and many more. All of these tools allow for some form of
structuring the data. For example, file systems allow for a hierarchical view
on data using directories and files, relational database management systems use
a tabular layout in which data can be structured using rows and columns, and
key/value stores build up a finite mapping. Giving structure to the data using
one of the systems greatly helps efficiently manipulating data. Unfortunately,
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
RDBMSs and object in an object-oriented language to derive an automated mapping
from and to the database. Automating the conversion step can greatly save time
in the development process.

Unfortunately, the same ORM technique is not applicable for functional
programming languages like Haskell. Haskell uses algebraic datatypes to
structure data, not objects described by classes like OO-languages.  There are
currently several ways of making data structures in Haskell persistent on an
external storage devices, but unfortunately all of these methods have some
limitations. 

We will now discuss three commonly used techniques for data persistence in
Haskell and explain the limitations of these approaches.

\begin{itemize}

\item \textbf{Relational databases.}
There are several packages available for Haskell that use connections to
existing relational database management systems (RDBMSs) to store values
outside of application memory. Either using manually written conversions
functions or via a generically derived conversion, values of arbitrary types to
database rows. In the related work section \ref{sec:relrdbms} some examples can
be found of database connectors for Haskell.

Unfortunately this mapping from algebraic datatypes to the table based layout
of RDBMSs, without losing any structural information, tends to be rather hard
and inefficient. Because the mapping from complex hierarchical Haskell data
structures to a table based database system is hard, most database connectors
only support a restricted set of types to be marshalled. This observation leads
to the conclusion that writing a Haskell data model that describes the
structure of an existing relational database is often very easy, but
\emph{using a relational database to store the values of the data model of an
existing Haskell program can be rather hard.}

\item \textbf{Key/value storage.} Similar to the connectors to relational
database are the mappers to key/value based storage systems. These systems
provide an interface similar to the Haskell |Map| datatype, a finite mapping
from keys to value. This interface can be very useful when managing a large
amount of records that can be easily identified by a single key. Lookup of
records by key can often be done very efficiently. Example of such key/value
stores can be found in the related work section \ref{sec:relkeyval}.

While key/value stores can be really useful, and are often rather easy in
usage, they have some limitations. \emph{Key/Value stores only allow one data
structure to be used, a finite mapping between keys and value.} When your
application data is structured differently either you have to write conversions
functions, when possible, or you have to search for another solution.

\item \textbf{Textual and binary serialization.}
Another possibility to store values of arbitrary datatypes outside the
application memory is to serialize the entire value to a textual or binary
representation. This representation can be written to and read from disk at
once.

The infamous |Show| and |Read| type classes in Haskell are primitive examples
of such a tool. These classes are used to print to and parse from a textual
representation and can be derived for most values generically by the compiler.
More advanced tools exist that use binary serialization to perform the same
trick more space and time efficient. Some of the libraries are very fast and
make use the types of values to prevent creating to much administrative
overhead when saving the binary data.

\emph{The big disadvantage of these libraries is that values can only be
written and read at once.} Due to this property, these techniques do not scale
well when dealing with very large amount of data. 

\end{itemize}

So there are several common ways in Haskell to save the application data on a
persistent storage like a hard disk. Unfortunately, all of these approaches
have limitations. In this document we will design and implement a framework for
data persistency in Haskell that without the limitations of the current
systems. Before we will introduce this framework we will identify three
different properties of a storage system that we think are important to have.

\begin{itemize}

\item \textbf{Generality.}
First we need a storage system to be generic.
Generic in this context means that we should be able to store Haskell values of
any Haskell datatypes. We do not want to limit the structure to a table based
layout or a key/value stores. Every data structure normally expressible in the
host language Haskell should be applicable to be made persistent.

\item \textbf{Incrementality.}
We want a storage framework to allow incremental access to the data.
Incrementality means it should be possible to perform operations on your data
set -- like updates, insertions and queries -- without reading the entire data
store into memory. Ideally the amount of read and write actions should be
minimized as much as possible, because disk access is significantly slower
compared to memory access. Incremental access is an important feature for
writing programs that scale well to large data sets.

\item \textbf{Transparency.}
A generic storage framework should be transparent to both the application
developer and the designer of the data structures that will be made persistent.
Writing the data structures and using these structures to store and retrieve
values from a persistent store should not be very different from using
in-memory data structures. Ideally it should be possible to implement data
structure in a purely functional way without prior knowledge of the persistence
framework and convert these to persistent variants automatically.

\end{itemize}

\begin{section}{Overview}

This document will describe the design and implementation of a generic storage
framework for the functional programming language Haskell. The framework is
implemented as a Haskell library and which is called
\emph{Islay}\footnote{After the Scottish island of Islay, the source of some
very fine whiskys.}. The source code of the library is available online on:

\begin{center}\url{http://github.com/sebastiaanvisser/islay}\end{center}

Additionally, the sources of this document can be found:

\begin{center}\url{http://github.com/sebastiaanvisser/msc-thesis}\end{center}

At the moment of writing the library is only a prototype and not in
release-worthy state. When the library matures it will eventually be globally
released on the public Haskell packages storage \emph{Hackage}. This
documentation is a description of the ideas and techniques from this library,
not of the actual implementation. This means that certain parts of the code and
examples may not correspond to the library exactly.

\end{section}

\begin{section}{Motivating example: spatial indexing}

The advantage of relational databases is the generality when it comes to
storing data. Using (or misusing) the generic table based layout of an RDMS
will almost always ensure that your application data can be saved in
\emph{some} structure.  Unfortunately it is not always easy to perform fast
queries over data when the structure of information in the table based layout
does not fit the algebraic layout of the original ADT well.

To illustrate this disadvantage, consider an application that stores a mapping
from two-dimensional geometrical coordinates to business relations.  Storing
such a mapping in a database is very simple and does not take a very
complicated structure. The problem arises when you want to perform efficient
spatial queries over the data, like getting the |n| nearest business relations
to the city center of Utrecht. Efficient SQL queries that perform such specific
tasks might be very hard to write. 

On the other hand, there are several data structures very capable of performing
such spatial queries very efficiently.  A quadtree is a domain specific data
structure specialized for efficient indexing of spatial information.  Quadtrees
are specializations of multidimensional search trees\cite{multitree}.  Elements
inside a quadtree are saved and indexed based on their geometrical coordinates
which results in very efficient spatial lookups.  Finding the |k| elements
nearest to a specific elements using a quadtree can be done in not more than
$O(k$ log $(n))$ time.  Performing the same task with the same asymptotic
running time using SQL queries on an table based RDMS is very difficult and
probably requires a lot of knowledge about the internals of the database.

Storing data is something most databases do very well, performing efficient
domain specific queries over a collection of elements is probably best done
using a specialized data structure.  Storing a large collection of data outside
application memory and still being able to perform specialized queries over the
data is still an unsolved problem in most programming languages and
accompanying storage libraries.

The proposed framework solves the problem of persistent domain specific data
structures by separating the concerns of persistent storage and that
specialized data structures. By projecting the internal structure of container
datatypes to an isomorphic variant on disk specialized algorithms will still
be applicable with the same time and space complexities.  The framework enables
developers of container libraries to focus on the internal structure and allows
for writing efficient algorithms without worrying about the internals of the
storage layer.

\end{section}

\end{chapter}
