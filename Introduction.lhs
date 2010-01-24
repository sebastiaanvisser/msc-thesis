%include polycode.fmt

%if False

> module Introduction where

%endif

\section{Introduction}

In the chapter TODO we will explore recursive data types with explicitly
annotated fixed points. We will show how to create different annotation types
and associate functionality with these annotations. We show how an identity
annotation can be used to get back a data type isomorphic to a data type
without annotations and how a debug annotation can be used to print out traces
of generic traversals. 

In chapter TODO we implement two general morphisms that can be used to
algebraically process recursively annotated data structures, the
\emph{paramorphisms} and \emph{apomorphisms}. Using these two general morphisms
we can easily derive four additional more specific morphisms,
\emph{catamorphisms}, \emph{anamorphisms}, \emph{endomorphic paramorphisms} and
\emph{endomorphic apomorphisms}. We show how to implement all of these
morphisms with the annotations and annotation associated functionality in mind.

Performing operations over annotated structure might require traversals to be
performed inside some computational context. For example, all annotations
requiring side effects might run inside the |IO| monad. The strictness
behaviour of some context might differ from pure side-effect free traversals.
Chapter TODO explorers how to make paramorphic traversals lazy on the inside
and strict from the outside.

Writing operations over data structures in terms of morphisms is not always
easy, especially when composing several operations into one. Chapter TODO
explores how we can make algebras for paramorphisms into applicative functors
and get back an idiomatic way of composing algebras.

...

