%include polycode.fmt

\section{Motivation}

  There are currently several ways of making Haskell data structure persistent
  on an external storage devices. Long-lived information storage is an
  essential ingredient in a large amount of modern applications. Although there
  is a lot to learn from existing tools, there are currently no work available
  that satisfies the goals specified earlier.

  \subsection{Relational Database Management Systems}

    There are several packages available for Haskell that use connections to
    existing relationae database management systems (RDMS'es) to store Haskell
    values.  Generic views on algebraic data types provide enough information
    to generically map values of arbitrary types to database rows.

    Unfortunately, the mapping from algebraic data types to the table based
    layout of RDMS'es without losing any structural information tends to be
    rather inefficient. Relying on a such a heavy dependency as an external
    RDMS system only for making your Haskell values persistent outside
    application memory unnecessarily increases the complexity of the program
    architecture.

    This is why a program can benefit from a lightweight, pure Haskell,
    framework that focuses on the persistence of purely functional, algebraic
    data structures.

  \subsection{Binary serialization}

    Another possibility to store values of arbitrary data types outside the
    application memory is to serialize the entire value to a textual or binary
    representation. This representation can be written to and read from disk at
    once.

    The infamous |Show| and |Read| type classes in Haskell are primitive
    examples of such a tool, that print to and parse from a textual
    representation and can be derived for most values generically. More
    advanced tools exist that use binary serialization to do the same trick
    more space and time efficient. Some of the libraries are very fast and make
    use the types of values to prevent creating to much adminstrative overhead
    when saving the binary data.

    The big disadvantage of these libraries is that values can only be written
    and read as a whole, which does not scale well when dealing with very large
    amount of data. That is why there is a need for a framework that can use
    the same generic serialization techniques as these packages but can cut the
    big data structures into pieces that can be freely navigated without
    touching the entire structure.

