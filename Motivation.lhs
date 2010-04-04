%include polycode.fmt
%include thesis.fmt

\begin{chapter}{Motivation}

Management of long-lived information is an essential ingredient of a large
amount of modern applications.  There are currently several ways of making data
structures in Haskell persistent on an external storage devices.  Although
there is a lot to learn from these existing tools, there is currently no work
available that allows both incremental access to the data and which is truly
transparent to the user.

Incremental access to the data means it should be possible to perform
operations on your data set -- like updates, insertions and queries -- without
reading the entire data store into memory. Ideally the amount of read and write
actions should be minimized as much as possible because disk access is
significantly slower compared to memory access. Incremental access is an
important feature for writing programs that scale well to large data sets.

Having a framework that is transparent to the end user means that both writing
the container data structures and using these structures to store record values
should not be very different from using in-memory data structures. Ideally it
should be possible to implement data structure in a purely functional way
without prior knowledge of the persistence framework and convert these to
persistent variants automatically.

We will now describe the two most common techniques for data persistence
currently used in Haskell and subsequently sketch an example application that
motivates the need for the proposed framework.

\begin{section}{Relational Database Management Systems}

There are several packages available for Haskell that use connections to
existing relational database management systems (RDMSes) to store values
outside of application memory.  Generic views on algebraic data types provide
enough information to automatically map values of arbitrary types to database
rows.

Unfortunately the mapping from algebraic data types to the table based layout
of RDMSes without losing any structural information tends to be rather hard and
inefficient. Because the mapping from complex hierarchical Haskell data
structures to a table based database system is hard, most database connectors
only support a restricted set of types to be marshalled.

Relying on a such a heavy dependency as an external RDMS system only for making
your Haskell values persistent outside application memory unnecessarily
increases the complexity of the program architecture. Reducing the dependency
on third party tool can also be a reason why a program can benefit from a
lightweight and pure Haskell framework that can be included into the program
like any other library.

\end{section}

\begin{section}{Binary serialization}

Another possibility to store values of arbitrary data types outside the
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

The big disadvantage of these libraries is that values can only be written and
read as a whole, which does not scale well when dealing with very large amount
of data. That is why there is a need for a framework that can use the same
generic serialization techniques, but can cut the big data structures into
pieces that can be freely navigated without touching the entire structure. Such
a framework can still use these serialisation libraries internally for the
individual non recursive pieces.

\end{section}

\begin{section}{Example domain: spatial indexing}

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
data types to an isomorphic variant on disk specialized algorithms will still
be applicable with the same time and space complexities.  The framework enables
developers of container libraries to focus on the internal structure and allows
for writing efficient algorithms without worrying about the internals of the
storage layer.

\end{section}

\end{chapter}
