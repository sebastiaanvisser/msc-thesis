%include polycode.fmt

\section{Motivation}

\fancy{
There are currently several ways of making Haskell data structure persistent on
an external storage devices. Long-lived information storage is an essential
ingredient in a large amount of modern applications. Although there is a lot to
learn from existing tools, there are currently no work available that allow
both incremental access and modifications to the persistent structure and work
nicely with real functional data structures\cite{okasaki}.
}

\fancy{
This section described the two most common techniques for data persistence
currently used and Haskell and sketches an example application that motivates
the need for the proposed framework.
}

  \subsection{Relational Database Management Systems}

  \fancy{
  There are several packages available for Haskell that use connections to
  existing relationae database management systems (RDMS'es) to store Haskell
  values.  Generic views on algebraic data types provide enough information to
  generically map values of arbitrary types to database rows.
  }

  \fancy{
  Unfortunately, the mapping from algebraic data types to the table based
  layout of RDMS'es without losing any structural information tends to be
  rather inefficient. Relying on a such a heavy dependency as an external RDMS
  system only for making your Haskell values persistent outside application
  memory unnecessarily increases the complexity of the program architecture.
  This is why a program can benefit from a lightweight and pure Haskell
  framework that can be included into the program like any other library.
  }

  \subsection{Binary serialization}

  \fancy{
  Another possibility to store values of arbitrary data types outside the
  application memory is to serialize the entire value to a textual or binary
  representation. This representation can be written to and read from disk at
  once.
  }

  \fancy{
  The infamous |Show| and |Read| type classes in Haskell are primitive examples
  of such a tool. These classes are used to print to and parse from a textual
  representation and can be derived for most values generically by the
  compiler. More advanced tools exist that use binary serialization to perform
  the same trick more space and time efficient. Some of the libraries are very
  fast and make use the types of values to prevent creating to much
  adminstrative overhead when saving the binary data.
  }

  \fancy{
  The big disadvantage of these libraries is that values can only be written
  and read as a whole, which does not scale well when dealing with very large
  amount of data. That is why there is a need for a framework that can use the
  same generic serialization techniques, but can cut the big data structures
  into pieces that can be freely navigated without touching the entire
  structure.
  }

  \subsection{Example domain: spatial indexing}

  \fancy{
  The advantage of relational databases is the generality when it comes to
  storing data. Using (or misusing) the table based layout of an RDMS will
  almost always ensure that your application data can be saved in \emph{some}
  structure. Unfortunately, it is not always easy to perform fast queries over
  data when the structure of information in the table based RDMS layout does
  not nicely fit the algebriac layout of your original ADT.
  }

  \fancy{
  To illustrate this, consider an application that stores a mapping from
  two-dimensional geometrical coordinates to business relations.  Storing such
  a mapping in a database is very simple and does not take a very complicated
  structure. The problem arises when one wants to perform efficient spatial
  queries over the data, like getting the |n| nearest business relations to the
  city center of Utrecht. Efficient SQL queries that perform such specific
  tasks might be very hard to write. 
  }

  \fancy{
  On the other hand, there are several data structures very capable of
  performing such spatial queries very efficiently.  A quadtree is a domain
  specific data structure specialized for efficient indexing of spatial
  information.  Quadtrees are specializations of multidimensional search
  trees\cite{multitree}.  Elements inside a quadtree are saved and indexed
  based on their geometrical coordinates which results in very efficient
  spatial lookups.  Finding the |k| elements nearest to a specific elements
  using a quadtree can be done in not more than $O(k$ log $(n))$ time.
  Performing the same task with the same asymptotic running time using SQL
  queries on an table based RDMS is very difficult and probably requires a lot
  of knowledge about the internals of the database.
  }

  \fancy{
  Storing data is something most databases are very good at, performing
  efficient domain specific queries over a collection of elements is probably
  best done using a specialized data structure.  Storing a large collection of
  data outside application memory and still being able to perform specialized
  queries over this data is still an unsolved problem in most programming
  languages and accompanying storage libraries.
  }

  \fancy{
  This framework tends to solve this problem by separating the concerns of data
  storage and that of specialized data structures. By projecting the internal
  structure of container data types to an isomorphic variant on disk
  specialized algorithms will still be applicable with the same time and space
  complexities. The framework enables developers of container libraries to
  focus on the internal structure and allows for writing efficient algorithms
  without worrying about the internals of the storage layer.
  }

