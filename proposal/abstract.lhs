%include polycode.fmt

\begin{abstract}

Algebraic data types (ADTs) are a powerful way to structure data in Haskell,
functions can be used to process values of these ADTs.  When dealing with a large
collection of data that does not fit into computer memory at once, tricks have
to be used to persist data on external storage devices.  There are several
techniques for data persistence available for Haskell, unfortunately none of
them truly transparent to the user or not allowing incremental access.  This
document proposes a new persistence framework for Haskell that uses purely
functional data structures to manage long lived data on disk.  It projects both
the data types and the algorithms working on these data types to a persistent
storage.  By not using any external database tools the system remains
lightweight and does not compromise the functional paradigm.

\end{abstract}
 
