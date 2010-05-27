%include polycode.fmt
%include thesis.fmt

\begin{chapter}{Future Work}  
\label{chap:futurework}

In this document we have seen how to use generic programming for recursive
datatype to build a persistent storage framework in Haskell. There are plenty
of opportunities for extension of the framework. This chapter gives a quick
enumeration of topics that can be used to extend or improve this project.

\begin{section}{Encoding type hashes into serialized data}

The framework described in this document takes values from the pure and type
safe Haskell world and saves their binary representation to a flat file, this
way loosing all type information. This is only safe when one single invocation
of one single application touches the data, because only then the Haskell type
system can be used to enforce this.

Guaranteeing type safety in between session is much harder to achieve. This
problem can be solved by storing a simple cryptographic hash of the type of the
data together with the data itself. When reading the data back in this hash can
be used to verify whether the data is what is to be expected. A similar
technique has been described for the programming language Clean \cite{clean}.

\end{section}

\begin{section}{Incremental folds}

Catamorphisms, or the more general paramorphisms, can be used to describe
operations over recursive datatypes. Techniques exist to cache intermediate
results of these operations as an annotation at the recursive positions of the
datatypes. Caching intermediate values prevents recursive algorithms form
having to recompute a new value for the entire data structure when only certain
parts of the structure change.

Operations caching intermediate results are called incremental
folds \cite{incr}. Incremental folds can easily be projected to the persistent
data structures from our framework and saved together with non-recursive nodes.
This would allow users to have efficient queries over long-lived and
incrementally changing data structures.

\end{section}

\begin{section}{Using attribute grammars to write queries}

The attribute grammar system for Haskell \cite{ag} can be used as a DSL to
describe algebras for catamorphisms. The current system produces an entire
application that creates the datatypes, produces the catamorphisms and run the
algebras using these catamorphisms. It would be useful if could merely
abstract the algebras from the system and use these to write queries over the
persistent data structures.

Recent work has shown that it also possible to create a first-class attribute
grammar system \cite{fly} for Haskell. First class AG systems might even be
easier to use as a front-end for our algebraic operations.

\end{section}

\begin{section}{Parallel access to independent sub structures}

A transactional cache as described in section \ref{sec:tcache} can be used to
guarantee that no two concurrent threads can alter the same data structure at
the same moment. However, some recursive data structures can have independent
recursive sub structures to which concurrent access can be safe. It might be
interesting to research if we can use the information about the recursive
structure of our datatypes to allow safe concurrent access to independent sub
structures.

\end{section}

\begin{section}{Extending the storage heap}

The storage heap could be extended in a way that allows custom variations
on how data is saved. Currently the heap only stores a plain binary stream of
data together with the byte size. Having a way to compress the data, encrypt
the data or additionally storing type hashes could be a valuable addition to
this project.

\end{section}

\begin{section}{Fixed point based optimizations}

One single IO operation can generally be performed more efficiently than a
large collection of smaller IO operations. When executing operations over large
parts of a persistent structure it might be desirable to load a block of nodes
into memory at once and perform a partial operation on this in-memory.

By using fixed points of a recursive datatype as a measurement of locality in
our data structures we might be able to store records that are near to each
other in the same I/O block. Operation which rely on the compositional
structure of the data, like algebraically defined catamorphisms, might
significantly benefit from such an optimization.

\end{section}

\end{chapter}

