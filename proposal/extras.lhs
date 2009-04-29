%include polycode.fmt

\section{Additional topics.}

Because it is hard to predict how much time it will take to implement an
initial basic framework and the additional possibilities are near infinite,
here is a list of additional work that can be done.

\subsection{Encoding type hashes into serialized data}

This framework takes type values from the pure and type safe Haskell world and
saves an binary representation to a flat file, this way loosing all type
information. This is safe when only one single invocation of one single
application touches the data, the Haskell type system can this way be used to
enforce type safety.

Guaranteeing type safety in between session is much harder to achieve. This
problem can be solved by storing a simple cryptographic hash of the type of the
data together with the data itself. When reading the data back in this hash can
be used to verify whether the data is what is to be expected. This is not a new
trick and is described in \cite{clean}.

\subsection{Nested data structures}

It is hard to predict whether it is very easy to extend the framework to allow
the persistence of nested data types. It requires some research to figure out
what adaptations, if at all, are needed to be able to store for example a
binary tree of lists of records.

\subsection{Incremental folds}

Catamorphisms, or the more general paramorphisms, can be used to describe
operations over recursive data structures. Techniques exist to cache
intermediate results of the operations in annotation in the recursive point of
the data structure. This prevents the algorithms form having to recompute a new
value for the entire data structure when only certain parts of the structure
change.

These so called incremental folds could easily projected to the persistent data
structures and saved together with non-recursive nodes. This would allow users
to have very efficient queries over long-lived, incrementally changing data
structures.

\subsection{Using attribute grammars to write queries}

The attribute grammar system for Haskell\cite{uuagc} can be used as a DSL to
describe algebras for catamorphisms. The current system produces an entire
application that creates the data types, produces the catamorphisms and run the
algberas using these catamorphisms. It would be very useful if could merely
abstract the algebras from the system and use these to write queries over the
persistent data structures.

Recent work has shown that it also possible to create a first-class attribute
grammar system for Haskell. It might possibly be easier to use this system to
write the algebras.

\subsection{Parallel access to independent sub structures}

The transactional cache described above will be used to guarantee that no two
concurrent threads can alter the same tree at the same moment. Some recursive
data structures though can have independent recursive sub structures to which
concurrent access can be safe. It might be interesting to research if we can
use the information about the recursive structure of our data types to allow
safe concurrent access to independent sub structures.

\subsection{Aspect oriented storage framework}

Make the storage heap extendible in a nice way to allow custom variations on
how data is saved. This should allow users to encrypt or compress individual
data blocks. This extensions should also be able to incorporate the type hashes
as described above.

