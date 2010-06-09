\section{Further work}

\subsection{Laziness}
\subsection{Sharing}

\section{Related work}

\subsection{Generic storage in Clean}

In their paper \emph{Efficient and Type-Safe Generic Data Storage} Smetsers,
Van Weelden and Plasmeijer \cite{clean} describe a generic storage framework
for the programming language Clean. Similar to our storage framework, they aim
at generically mapping functional data structures to a persistent storage on
disk. Using something similar to our storage heap -- they call this
\emph{Chunks} -- they are able to persist individual parts of the data
structures on disk without the need for reading and writing the entire
collection at once.

The mayor difference between their approach and ours is that they do not slice
the data structure at the recursive points but at the points where the actual
element values are stored. This means every record value is stored in its
own chunk, while they entire data structure itself is stored in one single
chunk. Updates of individual record values can now be done efficiently without
touching the entire collection, but for every structural change to the
collection the chunk containing the data structure itself --- they call this
the \emph{Root chunk} --- has to be read in and written back as a whole.


