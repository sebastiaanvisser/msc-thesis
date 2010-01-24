%include polycode.fmt
%include thesis.fmt

\begin{abstract}

Algebraic data types (ADTs) are a powerful way to structure data in Haskell
programs, functions can be used to process values of these ADTs.  When dealing
with a large collection of data that does not fit into computer memory at once,
tricks have to be used to make the data persistent on external storage devices.
There are two important properties that frameworks for data persistence at
least should have.  First of all it should allow incremental access to parts of
the data. By not requiring the entire data structure to be read from and
written to disk at once the tool will scale well to large amounts of data.  The
second important property is that users of the system should not be bothered
with the fact that the actual storage is located outside the application
memory. A system that is truly transparent to the user will better fit the
functional paradigm and therefore more easily be adapted. These two properties
are essential for a persistence framework in order to be useful in practice.
This document proposes a new persistence framework for Haskell that uses purely
functional data structures to manage long lived data on disk.  It projects both
the data types and the algorithms working on these data types to a persistent
storage.  By not using any external database tools the system remains
lightweight and does not compromise the functional paradigm.

\end{abstract}
 
