%include polycode.fmt

\section{Goals}

  For this thesis project to succeed at the least the following topics should
  have been researched and implemented as part of the framework.

  \begin{itemize}

    \item An efficient file-based storage heap that can grow and shrink on demand.

    \item An aspect oriented framework to lift data structures to persistent
    variants. Space and time complexity should be respected.

    \item At least one reasonably fast general purpose data structure, e.g.
    finger-tree or b-tree.

    \item At least one domain specific data structure, e.g. space partition
    tree.

    \item A garbage collector to cleanup unused nodes.

    \item An transactional memory cache to allow concurrent access.

    \item An example application and database to show the possibilities and
    benchmark.

  \end{itemize}

  \subsection{Heap}

    The storage heap will be the back-end of the persistence framework a will
    manage all low level IO operations. The interface of the heap should at
    least contain the following functions.

>store     :: Binary a =>  a ->               Storage t (Pointer a)
>retrieve  :: Binary a =>  Pointer a ->       Storage t a
>delete    ::              Pointer a ->       Storage t ()
>reuse     :: Binary a =>  Pointer a -> a ->  Storage t (Pointer a)

    All these functions operate inside the |Storage| monad, which runs inside
    the |IO| monad and has access to the underlying heap structure using a
    |State| monad. There is quite some freedom in the exact implementations of
    these functions which may significantly affect performance. We expect all
    of these function to run with the same time and space complexity as there
    in-memory equivalents so the expected running time of persistent algorithms
    matches the running time of the in-memory algorithms.

    The |Store| function takes a value of any type that we can generically
    serialize to a binary representation and allocates a fresh block on the
    heap, stores the binary representation and returns a pointer to this block.
    The |Pointer| data type is indexed with the type that is stored so we can
    later on use it only to read back the value of the correct type.

    The |Retrieve| function takes pointer to a value of some type that we know
    of we can deserialize from a binary representation and reads the value from
    the heap. Internally it will read the binary representation from the heap
    and deserialize the stream to a real value of the right type.

    The |delete| functions frees an existing block, making the previously
    occupied space available for future allocations. The |reuse| functions
    tries to reuse the existing space for a new value when possible, or
    reallocates a new block when the existing block does not contain enough
    space.

  \subsection{Persistence framework}

  We need a framework that helps developers of custom domain specific container
  data structures to lift their operations into the |Storage| monad. This
  framework is described in section [todo].

  The primary goals is to have at least the ability to generically lift
  morphism based functions to persistent operations. This would most certainly
  imply using some generic programming library to derive monadic variant of
  cata-, ana- and paramorphims and accompanying algebras and coalgebras.

  Because the one of the purposes of this project is to investigate the
  possible ways of writing recursive agnostic functions of inductively defined
  data types, the framework should not rely on a single way of lifting
  data structures and algorithms.

  \subsection{General purpose data structure}

    We should at least have one sufficiently efficient general purpose data
    structure. A simple choice for this would a dictionary based on a balanced
    binary tree similar to the Haskell |Data.Map| implementation.

    Another interesting general purpose data structure with a nice functional
    structure and good efficiency is the finger tree\cite{fingertree}. Finger
    trees are the based of the Haskell |Data.Sequence| data structure.

    \subsection{Domain specific data structure}

    The prove the real power of the system it is desirable to have at least one
    special purpose data structure included. Some domain specific data
    structures are very hard to map to relational database management system,
    while this framework should be able to handle them well.

    An example of such a domain specific data structure could be space
    partition tree, like quad trees or oct trees. These can be used to
    efficiently store and retrieve geometric data. Another interesting data
    type would be a trie -- or affix tree -- based dictionary. 

  \subsection{Garbage collector}

    Sharing in Haskell makes it possible that certain parts are included in the
    same data structure more than once. This has the implication that
    modification of persistent data does not allow us to unconditionally throw
    away or overwrite the original value, because these values might be shared
    among other parts of the structure.

    To make sure that the storage heap does not grow with every modification we
    are in need of a garbage collector to clean up unused blocks of data. 

    Todo: make some kind of prediction on what has to be changed in order to
    perform garbage collection. Do we need to able to identify and follow
    pointers within the storage blocks.

  \subsection{Transactional cache}

    To allow safe concurrent access to the underlying storage layer it might be
    desirable to have some form of transactional cache. This is a single entry
    point that runs operations on the persistent data structure using an
    in-memory transaction. Using transactional cache all operations will be
    performed in-memory can atomically be committed to the underlying storage
    heap.

    This layer can be used for both transactional access to the persistent data
    structure, but can also be used as a fast-access in-memory cached view on
    the data. In-memory operations are far more efficient than true disk
    accesses.  Frequently and recently used blocks of data can be cached
    in-memory to allow speedups of otherwise IO intensive operations.

  \subsection{Example application}

    To illustrate that the use of the framework and to be forced to think about
    the exposed API at least one demo application should be build. This program
    should illustrate the use of the underlying framework by storing a single
    data structure containing a reasonable large amount of records. All common
    operations should be included in the demo, like creating a storage from an
    existing list of records, inserting new record, querying records by some
    index, removing records and counting the number of records.

    This example application can also be used for testing and benchmarking the
    performance of the framework. Creating a set of test cases for the
    framework and profiling these in an automated way can be very helpful when
    trying to improve performance.

