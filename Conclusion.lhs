%include polycode.fmt
%include thesis.fmt

\begin{chapter}{Conclusion}
\label{chap:conclusion}

In this document we have described a storage framework for algebraic data types
in Haskell. Using the advanced Haskell type system in combination with generic
programming techniques we have build a generic annotation system for recursive
data structures. Annotation can be used to associate custom functionality with
the construction, destruction and modification of recursive data structures. We
have seen that using an identity annotation yields a data structures that runs
in-memory. Switching the identity annotation with a pointer annotation yields a
data structure that run on a storage heap on disk. 

Haskell's powerful type system has allowed us to create a very clean separation
between the storage layer and the persistent data structures. By writing
operations on recursive data structure as algebras that abstract away from
recursive we are able to plug-in custom actions. This allows us to write data
structures that can transparently be stored on a persistent disk without
writing explicit I/O code. The absence of I/O specific code makes it much more
easy to reason about the correctness of the framework.

Although writing operations on recursive datatypes as algebras and coalgebras
is a common functional programming technique that has been well described in
literature, writing algebraic computations in practice can be hard. It requires
a slight paradigm shift to abstract away from recursion, but we think it is
worth the benefit.

The final interface to the user is very natural and comes close to what one
might expect from working with in-memory data structures. The only drawback is
that all operations are lifted to work in a monadic context. This context
forces us to write composition of operations using monadic combinators instead
of regular function composition. This limitation is very common when
generically lifting functionality to different computational contexts in
Haskell. 

The annotation framework described in the first five chapters makes the
assumption that the recursive datatypes are regular recursive datatypes.
Datatypes making use of higher order recursion cannot be used. Extending the
system to also work for indexed datatypes has showed not to be particularly
hard, but did require almost duplicating the entire framework. Because the
mayor type mismatch all the involved helper datatypes and type classes have to
reimplemented with slightly different types. Unfortunately we do not know of a
way to build an abstraction layer hiding the differences between regular
datatypes and higher order datatypes. It also not clear whether this project
could benefit from such an abstraction.

Although generic programming for higher order datatypes has been well covered in
literature is not always easy. Especially the combination of higher order
algebras for annotated recursive operations is far more involved than working
with the regular recursive datatypes. The implementations of higher order
operations might require additional boilerplate code for proof terms and data
constructors for type level functions, fuzzing the core idea of the functions.
Although the implementation of the data structures becomes much harder, the
interface to the end-user does not change accordingly. We believe the benefits
of enforcing structural properties of data structures in the their types is
worth the extra effort.

In the end we think that the framework described in this document is a valid
contribution to the list of storage solution for Haskell.

\end{chapter}

\chapter*{Acknowledgements}
\label{chap:acknowledgements}

I would like to thank people.


