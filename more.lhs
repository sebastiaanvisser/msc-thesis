%include polycode.fmt

\section{Problems and extensions}

\draft{What storage heap can be created to efficiently allocate blocks of data
on demand? How can be learn from existing in-memory heaps? Do we need garbage
collecting of unused blocks, or do we exactly know when to free blocks by
analysing the behaviour of modifier functions? What would the smallest
interface needed for an abstract interface?}

\draft{Can we improve the system by caching or fusing several read/write
actions in-memory into one single action that shares common IO actions? Can
this fusion be expressed implicitly.}

\draft{The system described allows for an aspect oriented way of programming by
generically annotating data structure and algorithms working on them. Can be
capture the differetn aspects in the type signature of the final system. Is
this system composable? E.g. can be created a |FTree (Persistent, (Caching,
(Logging, Remote)}

\draft{What containers can we write to efficiently store and lookup records?}

\draft{Can we even use existing generic programming libraries to lift existing
container data structures to forms that can be annotated? Maybe by simulating
these in memory on partial data strucuters?}

\draft{What happens when we want to store infinite data structures with proper
sharing?}

\draft{What can we do to serialize even arbitrary computation? We might be able
to offer in-memory thunks as regular ADTs to the system and let them serialize
computations as well.}


