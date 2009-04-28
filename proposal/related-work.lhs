%include polycode.fmt

\section{Related work}

\subsection{Clean}

  In their paper \emph{Efficient and Type-Safe Generic Data Storage} Smetsers,
  Van Weelden and Plasmeijer\cite{clean} describe a library for generic storage
  for the programming language Clean. They also generically map pure and
  functional algebraic data structures to persistent data storage on disk.
  Using something similar as our storage heap -- they call this \emph{Chunks}
  -- they are able to persist individual parts of the data structures on disk
  without reading or writing the entire collection. The big difference in their
  approach is that they do not slice the data structure at the recursive points
  but at the points where the actual record values are stored. This means that
  every record value is stored in its own chunk, while they entire data
  structure itself is stored in one single chunk.  Destructive updates of
  individual record values can now be done efficiently without touching the
  entire collection. But, for every change in the structure of the data the
  chunk containing the data structure itself -- they call this the \emph{Root
  chunk} -- has to be read in and written back as a whole.

  Because of the different method of slicing the data they do not pose the same
  problems in lifting the pure structures to a persistent variant. This
  significantly simplifies their implementation while making the system less
  flexible.

  todo: they generically encode type into chunk.

  Find out: How are destructive updates possible when record size varies.

\subsection{Binary serialization}

  \emph{Data.Binary}\cite{databinary} is a library developed by Don Stewart
  that enables generic serialization Haskell values of arbitrary types to
  binary streams. The library uses type classes to be polymorphic in the values
  to be serialized/deserialized. Out of the box the library forces users to
  write the type class instances for their data types by hand, but luckily
  there are several ways this can be done generically.

  The Haskell \emph{derive}\cite{derive} package can be used to derive
  Data.Binary (and more) instances automatically for custom data types. The
  package uses (GHC only) Template Haskell for the derivation process but
  produces portable Haskell 98 code that can be linked manually into your
  project.

  Using generic programming techniques like \emph{EMGM}\cite{emgm},
  \emph{SYB}\cite{syb} and the \emph{MultiRec}\cite{multirec} library it is
  possible to generically derive instances as well. The advantage is that this
  can be used as a library and does not require external inclusion into your
  project. It might be interesting to not that using generic programming
  libraries -- of which there are quite a few \cite{compgen} -- it should not
  be that hard to skip the Data.Binary library entirely and write the functions
  from scratch, as described in \cite{printparse} and \cite{clean}.

\subsection{Algebras}

  In their paper \emph{Functional Programming with Bananas, Lenses, Envelopes
  and Barbed Wire} Meijer, Fokkinga and Paterson\cite{bananas} show how cata-,
  ana-, hylo- and paramorphisms can be used to manipulate values of recursive
  data types without explicitly going into recursion. By using algebras to
  describe what actions to perform at certain points in the recursive process,
  they only need a few basic combinators (like fold/unfold) to handle the
  actual recursion.

  This is a very well explored and common trick in functional programming that
  will also be extensively used in this project. By writing algebras for data
  type specific folds, the container data remain open for annotation.

  \subsubsection{Attribute Grammars}

  Attribute grammars can be seen as a way of writing catamorphisms. The
  attribute grammar system for Haskell\cite{ag} (\emph{UUAGC}) allows users to
  write operations over data structures in an aspect oriented way without
  explicitly coding the recursion. The different aspects written down using
  such an AG system can potentially be used as a recipe for query functions
  over our persistent data structures.

\subsection{Iteratee based IO}

  In Haskell, being a lazy language, the use of lazy IO has been quite common.
  Reading and processing the entire contents of a file lazily means there is no
  need to read the entire file into memory before processing it. Lazy IO has
  the awkward property that apparently pure Haskell expression have real world
  side effect and are therefor considered to be impure and `not in the spirit
  of Haskell'.

  To solve this problem Oleg Kiselyov has been working on left-fold enumerator,
  also known as iteratee-IO\cite{streams}. Iteratee based IO uses \emph{fold}
  like functions for high-performance, incremental IO without the property that
  the evaluation of pure code cause side effects. A library for iteratee based
  IO is now available for Haskell\cite{iteratee}.

\subsection{Sharing / Garbage Collection}

  Lots of research and work \cite{gengc,pargc} has been done in the field of
  garbage collection for pure and lazy functional programming languages like
  Haskell.  Lots of these techniques should be applicable to storage heaps
  outside the conventional computer memory, but located on disk.

\subsection{STM}

  Software Transactional Memory is a technique that is used to allow
  transactions for the access to shared memory. In their paper \emph{Composable
  Memory Transactions} Harris, Marlow, Peyton Jones and Herlihy\cite{stm}
  describe an implementation of transactional memory for Haskell on top of
  \emph{Concurrent Haskell}\cite{conchask}. The STM implementation for Haskell
  is composable, which means that separate block of atomic code can be composed
  into one larger block of atomic code. These atomic blocks of code can be used
  to give separate threads/processes exclusive access to certain memory
  variables.

  STM can not only be used to manage atomic actions to shared resources in
  memory, the abstraction can also be lifted to manage other program resources.
  This concept is implemented by the \emph{TCache}\cite{tcache} package
  implements this concept by using STM to provide a transactional cache to
  arbitrary persistent back-ends.
  
  These ideas can be used to allow transactional access to our persistent data
  structures when working with concurrent programs.

