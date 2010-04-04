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

\begin{chapter}{Generic storage}

In this section we will describe how to use the generic annotation framework
for recursive datatypes together with the file based storage heap to build a
generic storage framework. We use the |Pointer| type to create an annotation
that maps non-recursive nodes to individual storage blocks on the storage heap.
Operations working on the recursive datatypes will be lifted to one of the heap
contexts. These lifted operation will now no longer operate in-memory but read
there input, and write back their output, to an on-disk representation.

\begin{section}{Pointer annotation}

In the previous section about the heap we have seen the |Pointer| type
that stores an integer offset into the storage heap. When we want to represent
recursive datatypes that live on our heap we can use these pointers as the
annotations for our fixed points. Using pointer at the fixed points slices a
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

\end{section}

\begin{section}{Annotation Instances}

In the section dealing with generic traversals over recursive datatypes we
have, among other morphisms, implemented annotation aware paramorphism and
apomorphism. These traversal functions can be used to both destruct and
construct recursive datatype with annotated fixed points. For the binary tree
example we seen some simple algebras and coalgebras to describe the semantics
of the traversal. In order to be able to apply these algebras to our persistent
binary tree we have to make the |P| type an instance of the annotation type
classes.

We first make |P| an instance of the |AnnOut| type class. We can simply
use the |read| function from our read-only storage heap for the implementation.
This read-only annotation can be associated with the |HeapR| context. We first
unpack the fixed point and the |P| wrapper and than read one non-recursive node
from disk. Because the storage heap only stores plain strings of bits a
|Binary| instance is needed to deserialize the Haskell value. The |annOut|
function takes a pointer to one non-recursive node, possibly containg pointers
to other nodes again, and return this node, with type |f (FixA P f)|. When the
fixed point does not contains an annotation at all we just unpack the node like
we did with the other annotations instances,

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnOut P f HeapR where
>
>   annOut (InA (P  f)  ) = read f
>   annOut (InF     f   ) = return f

The |AnnIn| instance for the |Pointer| type is just a matter of writing a
single non-recursive node to disk and storing the pointer inside the |P|
wrapper type. This action can only be done inside the read-write |HeapW|
context.

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnIn P f HeapW where
>
>   annIn = fmap (In . P) . write

Sometimes, when working inside the read-write heap context |HeapW|, we also
want to be able to perform read-only actions, so we also give an |AnnO|
instance for the |Pointer| type inside the |HeapW| context. The implementation
is very similar to the one for the read-only |HeapR| context, except we no lift
teh |read| operation to the read-write context.

\todo{what about laziness? |liftLazy| instead of |liftR|?}

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnOut P f HeapW where
>
>   annOut (InA (P  f)  ) = liftR (read f)
>   annOut (InF     f   ) = return f

Because we now both have an |AnnO| and an |AnnI| instance for the |Pointer|
type inside the |HeapW| context we can create an instance for the |AnnIO| typeclass.
We can imagine giving a more specific efficient instance in the future, but for
now we just derive the instance automatically.

> instance  (Traversable f, Binary (f (FixA P f)))
>       =>  AnnIO P f HeapW where

In order to store the recursive structures on disk we also need a |Binary|
instance for the annotated fixed point operator type itself. This is a partial
instance because we will not allow to store unannotated fixed points. This
means the structure should first be fully annotated using the |fullyIn|
function.

\todo{introduce |fullyIn|}

> instance Binary (a f (FixA a f)) => Binary (FixA a f) where
>
>   put = put . outa
>   get = liftM InA get

\end{section}

\end{chapter}

