%include polycode.fmt

\section{Project goal}

The project is quite ambitious in the sense that there are more than a handful
features and optimizations possible that are not essential to get an initial
system working. The initial goal of this project is to have at least the
following properties.

\begin{itemize}

  \item
  The system should be able to store values of arbitrary Haskell types
  generically. This means the programmer does not have to write their own
  marshalling code.

  \item
  The storage can be structured using functional data structures as are
  commonly used for maintaining and manipulating data in-memory.  This would
  imply that existing algorithms can be uniformly lifted to work on persistent
  data. Developers should be able to make there data persistent at disk in
  their favourite domain specific container type, like finger trees, trie based
  maps, geometric quad trees, lists, etc.

  \item
  The implementation of container data structures specialized for the
  persistent machinery should be as easy as possible. Developers should not be
  bothered by the inner workings of the system. Both developers and users
  should not be bothered with |IO| actions when dealing with pure functions on
  pure data.

  \item
  It should be possible to freely navigate the persistent data structure
  without reading and writing the entire storage at once. For example,
  performing a find action on an persistent AVL tree should actually run in
  |O(log n)| and should not touch more data than necessary for this action.

  \item
  The system should be implemented in pure Haskell and should not rely
  on external database tools. 

\end{itemize}

The next sections explains the technical aspects that are needed in order to
compose the system. Because this document is still a proposal, some functions
are not yet implemented as displayed here and might need some additional
research.


