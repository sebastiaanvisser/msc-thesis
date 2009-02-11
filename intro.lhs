%include polycode.fmt

\section{Introduction}

\review{
Haskell is a powerful language for processing data. Algebraic data types (ADTs)
allow programmers to compose their data in a structured and type safe way,
functions can be used to process this data. All of this takes place in internal
memory, somewhere deeply buried in the application's storage heap.
Unfortunately there are currently no systems available for Haskell that allow
the programmer to process data stored on an external storage device with the
same power and flexibility as you would have when dealing with in-memory data.
}

\review{
Currently there are several ways to make Haskell data persistent on disk.
Unfortunately, all of the systems available lack some important properties or
do not fit the functional paradigm well. The following enumeration lists the
most common ways of persisting your data in Haskell today.
}

\begin{itemize}

  \item
  \review{
  There are good connectors to existing relational database management systems
  (RDBMS) available. These connectors allow you to directly query an RDBMS
  using the SQL query language. This gives developers the complete power of
  databases, like fast querying, indexing, transactions etc. 
  }

  \review{
  The problem with these systems is that is does not fit the functional world
  of algebraic data type very well. Making use of such a system implies you
  have to map all your data from and to database tables. Clever tricks must be
  used to map the structure of your ADT to database tables efficiently.
  }

  \review{
  \emph{Example Haskell packages for connecting to database management systems:
  HDBC\cite{hdbc}, sqlite\cite{sqlite}.}
  }

  \item
  \review{
  Others libraries build on top of these RDBMS bindings and allow marshalling
  of Haskell data generically. Using generic programming techniques they allow
  mapping values of any \footnote{Most libraries do have some limitation on
  what ADTs can be processed, for example only regular data types.} data type
  to database rows and vice versa.
  }

  \review{
  While these system really add some power, because developers do not have to
  write the marshalling code manually, these system are limited in their own
  way. Because RDBMSs are used as a back-end it is hard to map efficient domain
  specific functional data structures to the database world. Only real data
  records can be made persistent, the container structures in which you would
  normally manage your data are replaced by the internals of the database.
  }

  \review{
  \emph{Currently both Americo Vargas Villazon and Chris Eidhof\cite{chris} are
  working on (or making use of) generic bindings to existing database systems.}
  }

  \item
  \draft{
  Not all libraries for data persistency make use of RDBMSs, other libraries
  directly serialize Haskell data types to blocks of textual data or binary
  data. These blocks can be saved to- or read from file at once in a type safe
  way. This approach is rather lightweight, because it does not rely on an
  external systems and can be implemented in pure Haskell.
  }

  \draft{
  Everyone working with Haskell knows how to use the |Read| and |Show| classes
  to serialize and deserialize data to a textual form. This system is really
  useful for small systems, but is rather slow and uses far more space than is
  necessary. 
  }

  \draft{
  Newer systems allow serialization to binary form, which is far more space
  efficient than a textual representation. Such generic serialization is an
  easy approach to data persistency that is quite simple to implement and has
  good performance.
  }

  \draft{
  The major disadvantage of such systems is that only monolithic blocks of data
  can be stored to- or read from disk at once. These libraries do not allow
  partial updates of data and selectively querying for specific records. This
  can be big problem when dealing with large data sets.
  }

  \draft{
  \emph{Examples: Data.Binary package, Happs-state, generic-haskell read/write.}
  }

\end{itemize}

\draft{
This document proposes a new lightweight approach to data persistency in Haskell which 
is both flexible and functional. Inspired by the in-memory representation of
Haskell data types this systems will generically lift functional data
structures to work on a persistent data storage.
}

\draft{
The initial goal of the system will be to:
}

\begin{itemize}

  \item
  \draft{
  Store arbitrary Haskell values generically, which means users to do not have
  to write their marshalling code themselves.
  }

  \item
  \draft{
  The data storage can be structured using functional data structures as are
  commonly used for storing data in-memory in most Haskell programs. This means
  that existing algorithms can be uniformly lifted to work on persistent data.
  Store your date at disk in your favourite domain specific data structure,
  like finger trees, tries based maps, geometric quad trees, lists, etc.
  }

  \item
  \draft{
  The implementation of container data structures specialized for the
  persistent machinery should be as easy as possible. Developers should not be
  bothered by the inner workings of the system. Do not bother users and
  developers with |IO| actions when dealing with pure functions on pure data.
  }

  \item
  \draft{
  It should be able to freely navigate the persistent data structure without
  reading and writing the entire storage at once. Performing a find action on an
  persistent AVL tree should actually run in |O(log n)| and should not touch
  more data than necessary for this action.
  }

  \item
  \draft{
  The system should be implemented in pure Haskell and should not rely on
  external database tools. This keeps the system lightweight and gives us the
  possibility to keep the system `functional'. 
  }

\end{itemize}

\draft{
The next sections explains the technical aspects that are needed in order to
compose the system.
}

\section{Containers}

\draft{
In haskell you can have data types of different structure; \emph{primitive
values} like characters and integers, \emph{composed values} like a user
containing a name, address and country field, and \emph{container data types}
like lists, maps, trees, etc.
}

\draft{
When dealing with larger collection of data it is convenient to wrap your
primitive or composed values in a container, or a composition of multiple
containers.
}

\draft{
For example, a list of mappings from strings a binary tree of integers.
}

\draft{
>type MyCollection = [Map String (Tree Int)]
}

\draft{
Persisting all data in that collection, including the collection adminstration
itself, on the an external storage device and still want to apply regular
functions like lookup, update and delete on it transparently is a bit involved.
}

\draft{
Inspired by how data is stored in-memory and using some clever generic
programming techniques it should be possible to hide most details of
persistency from the user.
}

\begin{itemize}

  \item
  \draft{
  When need some sort of heap mechanism on the storage backend with which we
  can allocate new blocks of data and free unused block of data. We need to be
  able to serialize pointer values to these data blocks to allow references
  between different data values.
  }

  \item
  \draft{
  We need to be able to serialize and deserialize values of every data type
  generically to streams of bits generically.
  }

  \item
  \draft{
  We need to be able to track `read' and `write' actions of function working on
  collection algorithms to be able to lift these actions to the world of
  persistency. This lifting process should be invisible to the users of the
  system and at least be easy to use for writers of container data types.
  }

\end{itemize}

\section{Storage}

\draft{
When data needs to be stored on the external device and must be queried,
modified and extended on the fly without touching the entire data store at once
we needs something analoge to the heap used in-memory.
}

\draft{
>allocate  :: Heap -> Size -> IO Pointer
>free      :: Heap -> Pointer -> IO ()
>read      :: Pointer -> IO ByteString
>write     :: Pointer -> ByteString -> IO ()
}

\draft{
This interface should allow us to freely navigate, grow and shrink the data
wihtout touching the entire file. This is essential when dealing with large
amounts of data.
}

\section{Generic serialization}

\draft{
In order to be able to save more than just binary data we need to perform
binary serialization. This function should work for all data that we can view
generically.
}

\draft{
>serialize    :: View a => a -> BitStream
>deserialize  :: View a => BitStream -> Maybe a
}

\draft{
There are several libaries available for generic serialization. It might be
interesting to investigate which of them is most suitable to our needs.
}

\section{Persistent containers}

\draft{
The hardest part of the project will be the to allow the usage of pure
functional container data types as a projection on the persistency layer. Most
-- or possibly all -- container data structures are based on recursive data
types; data types that in one or more of their constructors refer back to
themeselve. By making both the recursive points in the container definitions
and the recursion in the container algorithms explicit using a fix-point
combinator, we can freely annotate the container's behaviour.  This means that
we are able to generically hook in our own functions where the original
algorithm would go into recursion. By hooking in functions that read or write
parts of the container and container data from file we can transparently
persist the data structure.
}

\draft{
To make things more clear, consider the following example of a binary tree
|FTree| parametrized with type variabel |f| for the recursive points. The tree
is parametrized with a key type |a| and a value type |b|.
}

\draft{
>data FTree a b f = Leaf | Branch a b f f
}

\draft{
Now we can, among other things, tie the knot using a fix-point combinator and
get a back a proper |Tree| data type indexed with the key and value types.
}

\draft{
>newtype Fix f = In {out :: f (Fix f)}
>newtype Tree a b = Tree { ftree :: Fix (FTree a b) }
}

\draft{
Now we are able to write the common binary tree lookup function with some
specialized machinery to deal with recursion. The function takes, among the key
to search for, a function to lift values into an annotated query result and a
function that deals with the recursive lookup.
}

\draft{
>lookupA :: (Ord a, Monad m) =>
>      a            -- key to search for
>  ->  (m b -> c)   -- lifter for query result
>  ->  (f -> c)     -- recursive annotated lookup
>  ->  FTree a b f  -- tree to search in
>  ->  c            -- lifted query result
>lookupA _ p _ Leaf = p (fail "not found")
>lookupA a p f (Branch c d l r) =
>  case a `compare` c of
>    EQ  -> p (return d)
>    LT  -> f l
>    GT  -> f r
}

\draft{
Because we left the choice for the recursive part of the |lookupA| 
function open, we can tie the knot on several ways. The following function
generically lifts a query function -- from a container type to a query results
-- to a monadic computation. This monadic computation can be parametrized with
custom processor functions that describes how we can get to the sub-containers.
}

\draft{
>monadicQ :: Monad m  -- Monad we live in.
>  => (t -> m a)      -- Lifted unwrap function.
>  ->   ((c -> m c)   -- Lift function.
>     -> (t -> m b)   -- Post processor.
>     -> a            -- Recursive container to query in.
>     -> m b)         -- Recursive lifted query result.
>  -> t               -- Container to query in.
>  -> m b             -- Lifted query result.
>monadicQ p q c = p c >>= q return (monadicQ p q)
}

\draft{
To make clear what can do with the combination of such a specific, annotation
aware, query function like |lookupA| and a generic monadic lifter function we
introduce the |lookupInIO| function. This function annotates the |lookupA| with
a monadic computation that prints out the piece of the container (sub-tree) we
are currently dealing with. The result is a lookup function that prints out a
trace of which part of the container it traverses.
}

\draft{
>lookupInIO :: (Ord a, Show a, Show b) =>
>  a -> Tree a b -> IO (Maybe b)
>lookupInIO a = monadicQ proc (lookupA a) . out
>  where proc = (\c -> print c >> return c) . out
}

\draft{
In the previous example we plugged in the |Fix FTree| into the recursive points
of |FTree| to get back a real binary tree. We can now plug in not a real tree
but a |Pointer| to a location of the sub-trees on the persistent Heap.
}

\draft{
>type PTree a b = FTree a b Pointer
}

\draft{
Now we should be able to use the same annotation technique as before, but this
time we do not unpack the fix-point and print a trace, but read the next
(partial) sub-tree from disk and continue the lookup.
}

\draft{
>lookupP :: Ord a => a -> PTree a b -> IO (Maybe b)
>lookupP = monadicQ (deserialize `liftM` read) . lookupA
}

\draft{
This trick will be the basis of a more or less transparent system for data type
persistency. Lots of research can be invested into finding ways to make the
usage as clean as possible without losing to much performance.
}

\draft{
In the example above we have only sketched how the systems work for query
functions, the same ideas can be used for producer and modifier functions.
}

\section{Problems and extensions}

\draft{
What storage heap can be created to efficiently allocate blocks of data on
demand? How can be learn from existing in-memory heaps? Do we need garbage
collecting of unused blocks, or do we exactly know when to free blocks by
analysing the behaviour of modifier functions? What would the smallest
interface needed for an abstract interface?
}

\draft{
Can we improve the system by caching or fusing several read/write actions
in-memory into one single action that shares common IO actions? Can this fusion
be expressed implicitly.
}

\draft{
The system described allows for an aspect oriented way of programming by
generically annotating data structure and algorithms working on them. Can be
capture the differetn aspects in the type signature of the final system. Is
this system composable? E.g. can be created a |FTree (Persistent, (Caching, (Logging,
Remote)))|?
}

\draft{
What containers can we write to efficiently store and lookup records? 
}

\draft{
Can we even use existing generic programming libraries to lift existing
container data structures to forms that can be annotated? Maybe by simulating
these in memory on partial data strucuters?
}

\draft{
What happens when we want to store infinite data structures with proper sharing?
}

\draft{
What can we do to serialize even arbitrary computation? We might be able to
offer in-memory thunks as regular ADTs to the system and let them serialize
computations as well.
}

