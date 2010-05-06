%include polycode.fmt
%include thesis.fmt

\begin{chapter}{Future Work}  
\label{chap:futurework}

In this document we have seen how to use generic programming for recursive data
type to build a persistent storage framework in Haskell. There are plenty of
opportunities for extension of the framework. This chapter gives a quick
enumeration of topics that can be used to extend or improve this project.

\begin{section}{Encoding type hashes into serialized data}

This framework takes type values from the pure and type safe Haskell world and
saves an binary representation to a flat file, this way loosing all type
information. This is safe when only one single invocation of one single
application touches the data, because the Haskell type system can be used to
enforce this.

Guaranteeing type safety in between session is much harder to achieve. This
problem can be solved by storing a simple cryptographic hash of the type of the
data together with the data itself. When reading the data back in this hash can
be used to verify whether the data is what is to be expected. A similar
technique for the programming language Clean has been described in
\cite{clean}.

\end{section}

\begin{section}{Nested data structures}

It is hard to predict whether it is very easy to extend the framework to allow
the persistence of nested data types. It requires some research to figure out
what adaptations, if at all, are needed to be able to store, for example, a
binary tree of lists of records. In order to store nested data type we are at
least in the need of a generic programming library that can handle this.
See \cite{initial}.

\end{section}

\begin{section}{Incremental folds}

\docite{sean on his work}

Catamorphisms, or the more general paramorphisms, can be used to describe
operations over recursive data structures.  Techniques exist to cache
intermediate results of these operations as an annotation at the recursive
positions of the data structure.  This prevents these algorithms form having to
recompute a new value for the entire data structure when only certain parts of
the structure change.

These caching operations are called incremental folds and could easily
projected to the persistent data structures and saved together with
non-recursive nodes. This would allow users to have very efficient queries over
long-lived and incrementally changing data structures.

\end{section}

\begin{section}{Using attribute grammars to write queries}

The attribute grammar system for Haskell\cite{ag} can be used as a DSL to
describe algebras for catamorphisms. The current system produces an entire
application that creates the data types, produces the catamorphisms and run the
algberas using these catamorphisms. It would be very useful if could merely
abstract the algebras from the system and use these to write queries over the
persistent data structures.

Recent work has shown that it also possible to create a first-class attribute
grammar system for Haskell. It might possibly be easier to use this system to
write the algebras.

\end{section}

\begin{section}{Parallel access to independent sub structures}

The transactional cache described above will be used to guarantee that no two
concurrent threads can alter the same tree at the same moment. Some recursive
data structures though can have independent recursive sub structures to which
concurrent access can be safe. It might be interesting to research if we can
use the information about the recursive structure of our data types to allow
safe concurrent access to independent sub structures.

\end{section}

\begin{section}{Aspect oriented storage framework}

Make the storage heap extendible in a nice way to allow custom variations on
how data is saved. This should allow users to encrypt or compress individual
data blocks. This extensions should also be able to incorporate the type hashes
as described above.

\end{section}

\begin{section}{Fixed point based optimizations}

One single IO operation can generally be performed more efficiently than a
large collection of smaller IO operations. When executing operations over large
parts of a persistent structure it might be desirable to load a block of nodes
into memory at once and perform a partial operation on this in-memory.

By using fixed point information as a measurement of locality in our data
structures we might be able to store records that are near to each other in the
same IO block. Operation which rely on the compositional structure of the data,
like algebraically defined catamorphisms, might significantly benefit from such
an optimization.

\end{section}

\end{chapter}

