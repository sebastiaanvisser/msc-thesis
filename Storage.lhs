%include polycode.fmt
%include thesis.fmt

%if False

> {-# LANGUAGE
>     KindSignatures
>   , GeneralizedNewtypeDeriving
>   , FlexibleContexts
>   , UndecidableInstances
>   , FlexibleInstances
>   , MultiParamTypeClasses
>   #-}
> module Storage where

> import Data.Traversable
> import Data.Binary
> import Fixpoints
> -- import Morphisms
> import Heap
> import Prelude hiding (read)

%endif

\begin{section}{Generic storage}

In this section we will describe how to use the generic annotation framework
for recursive datatypes together with the file based storage heap to build a
generic storage framework. We use the |Pointer| type to create an annotation
that maps non-recursive nodes to individual storage blocks on the storage heap.
Operations working on the recursive datatypes will be lifted to one of the heap
contexts. These lifted operation will now no longer operate in-memory but read
there input, and write back their output, to an on-disk representation.

\begin{subsection}{Pointer annotation}

In the previous section about the heap we have seen the |Pointer| type
that stores an integer offset into the storage heap. When we want to represent
recursive datatypes that live on our heap we can use these pointers as the
annotations for our fix points. Using pointer at the fix points slices a
recursive data structure into individual non-recursive pieces that are
indirectly connected using a |Pointer| instead of a regular in-memory
connection.

The |Pointer| datatype itself cannot be used as an annotation because it has
kind |* -> *|, while the annotations in our framework have kind |(* -> *) -> (*
-> *)|. To be able to still use pointers as annotations we define a simple
wrapper type |P| with the right kind.

> newtype P f a = P { unP :: Pointer (f a) }
>  deriving Binary

As an example, we can now represent a persistent version of our binary tree
with the following type.

> type PTree = FixA P Tree_f

In the section dealing with generic traversals over recursive datatypes we
have, among other morphisms, implemented annotation aware paramorphism and
apomorphism. These traversal functions can be used to both destruct and
construct recursive datatype with annotated fixed points. For the binary tree
example we seen some simple algebras and coalgebras to describe the semantics
of the traversal. In order to be able to apply these algebras to our persistent
binary tree we have to make the |P| type an instance of the annotation type
classes.

\end{subsection}

\begin{subsection}{Annotation Instances}

We first make |P| an instance of the |AnnQ| type class. Because we can simply
use the |read| function from our read-only storage heap for the implementation.
This read-only annotation can be associated with the |HeapR| context. We first
unpack the fixed point and the |P| wrapper and than read one non-recursive node
from disk. Because the storage heap only stores plain strings of bits a
|Binary| instance is needed to deserialize the Haskell value. The |query|
function takes a pointer to one non-recursive node, possibly containg pointers
to other nodes again, and return this node, with type |f (FixA P f)|.

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnQ P f HeapR where
>   query = read . unP . out



> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnP P f HeapW where
>   produce = fmap (In . P) . write






> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnQ P f HeapW where
>   query = liftR . read . unP . out

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnM P f HeapW where

> instance Binary (a f (FixA a f)) => Binary (FixA a f) where
>   put = put . out
>   get = fmap In get

\end{subsection}

\end{section}

