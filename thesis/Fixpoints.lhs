%include polycode.fmt
%include thesis.fmt

%if False

> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, UndecidableInstances #-}
> module Fixpoints where

> import Control.Applicative
> import Control.Category
> import Control.Monad.Reader hiding (mapM)
> import Data.Foldable
> import Data.Monoid hiding (Endo)
> import Data.Traversable
> import Prelude hiding ((.), id, mapM)
> import Data.Time.LocalTime

%endif

%if False

> fmap' :: Functor f => (a -> b) -> f a -> f b
> fmap' = fmap

> infixl 6 :+:
> infixl 7 :*:

> type a :+: b = Either a b
> type a :*: b = (a, b)

> class (Applicative m, Monad m) => AM m
> instance (Applicative m, Monad m) => AM m
> class (Applicative m, MonadIO m) => AMIO m
> instance (Applicative m, MonadIO m) => AMIO m

> fixp :: (t -> t) -> t
> fixp a = a (fixp a)

%endif

\begin{section}{Annotated fixed points}

% -----------------------------------------------------------------------------

\begin{subsection}{Fixed points}

\review{
Most container datatypes in Haskell are written down with explicit recursion.
An example of a container type using explicit recursion is the following binary
tree datatype. This binary tree stores both a value and two explicit sub-trees
in the branch constructor, empty trees are indicated by a leaf.
}

> data Tree_1 = Leaf_1 | Branch_1 Int Tree_1 Tree_1

\noindent
\review{
To gain more control over the recursive positions of the datatype we can
parametrize the binary tree with an additional type parameter used at the
recursive positions. Not the tree datatype itself, but the users of the
datatype may now decide what values to store as sub-trees. We call this new
datatype |Tree_f|, the tree functor.
}

> data Tree_f f = Leaf | Branch Int f f
>   deriving Show

\noindent
\review{
To get back a binary tree that is isomorphic to our original binary tree, in
that it stores actual sub-trees at the recursive points, we can use an explicit
fixed point combinator at the type level. This combinator, conventionally
called |Fix_1|, takes a type constructor of kind |* -> *| and parametrizes this
type with its own fixed point. \docite{fixpoint combinator}
}

> newtype Fix_1 (f :: * -> *) = In_1 { out_1 :: f (Fix_1 f) }

\noindent
\review{
By applying the fixed point combinator to the tree functor we get a back a true
binary tree again, with real sub-trees at the recursive positions.
}

> type Tree_2 = Fix_1 Tree_f

\noindent
\review{
We will call datatypes that abstract away from recursion using an additional
type parameter \emph{open datatypes}.
}

\review{
To make it easier to deal with the recursive structure of the binary tree we
can make the tree functor an instance of Haskell's |Functor| type class. The
functorial |fmap| takes the input function and lifts it to be applied against
the sub-structures of the binary tree.
}

> instance Functor Tree_f where
>   fmap _  Leaf            = Leaf
>   fmap f  (Branch v l r)  = Branch v (f l) (f r)

\noindent
\review{
Besides |Functor| Haskell has two additional type classes that help with
generic traversals over container data types. These are the |Foldable| and
|Traversable| type classes\footnote{Note that these type class instances are
very simple and mechanically derivable. The GHC Haskell compiler version 6.12.1
and above is able to derive the instances for |Functor|, |Foldable| and
|Traversable| for you automatically.}. The |Foldable| type class allows us to
reduce an entire structure into a single value using some |Monoid| operation. 
}

> instance Foldable Tree_f where
>   foldMap _  Leaf            = mempty
>   foldMap f  (Branch _ l r)  = f l `mappend` f r

\noindent
\review{
The |Traversable| type class, which requires |Foldable| as its super class,
allows a generic traversal over a structure while performing an actions for
each element. The actions performed are |Applicative| computations. The
|Traversable| instance for our binary tree example is a straightforward
preorder traversal. The actions are written down using idiom brackets.
\docite{idiom brackets}
}

> instance Traversable Tree_f where
>   traverse _  Leaf            = (| Leaf |)
>   traverse f  (Branch v l r)  = (| (Branch v) (f l) (f r) |)

\noindent
\review{
Having instances of the |Traversable| class around is very useful, because it
allows us to use the generic version of the Prelude's |mapM| function. This
function enables us to |fmap| a monadic action over a structure and transpose
the result.
}

> mapM1 :: (Traversable f, AM m) => (a -> m b) -> f a -> m (f b)

\noindent
\review{
The |mapM1| function can be used to perform a very lightweight form of generic
programming.
}

%if False

> mapM1 = undefined

%endif

\end{subsection}

% -----------------------------------------------------------------------------

\begin{subsection}{Annotations}

\review{
In the previous section we worked out some basic building blocks that can be
useful when working with container datatypes which are explicitly parametrized
with the recursive structures.  But why would it be useful to abstract away
from recursion in the first place? This section will show how we can store
additional information at the recursive positions of open datatypes using an
annotated fixed point combinator.
}

\review{
First we introduce a new fixed point combinator that stores an annotation over
a container data type instead of a datatype directly. This type level fixed
point combinator is called |FixA|. \footnote{ The |FixA| |newtype| might feel
redundant at first sight, because we could as well just parametrize the
original |Fix| with an annotated structure |(a f)|, yielding the same result.
From the usage of the |FixA| it has become clear that expressing the more
specific fixed point |Fix| in terms of the more general |FixA| helps us to more
easily reuse functionality later on.} Throughout this document the \emph{alpha}
postfix will be used to indicate that a type or a function is annotation aware.
}

> newtype FixA a f = In { out :: (a f) (FixA a f) }

\noindent
\review{
Note the kind of the annotation variable |a|, the annotation is applied to the
original container type |f| which has kind |* -> *|. Because the annotation
itself applied to the container type |f| needs to have the same kind as |f|,
the variable |a| has kind |(* -> *) -> (* -> *)|.
}

\review{
It is now we can very easy to describe a fully annotated binary tree by
applying the annotated fixed point combinator to the tree functor.
}

> type TreeA a = FixA a Tree_f

\review{
We now introduce the identity annotation, called |Id|, that stores no
additional information but just encapsulates the underlying container type.
}

> newtype Id f a = Id { unId :: f a }

%if False

>  deriving Show

%endif

\noindent
\review{
The identity annotation can be used to get back the regular fixed point
combinator defined in the previous section by plugging it into a |FixA|.
Because the identity annotation stores no additional information we call a |Fix
f| structure an unannotated or plain structure.
}

> type Fix f = FixA Id f

\noindent
\review{
Working with a fully annotated structure using the |FixA| combinator or working
with a plain structure using the |Fix| combinator both require all
sub-structures to be surrounded by an additional |In| constructor. To make the
usage of plain binary trees more easy we create a |Tree| type synonym and two
smart constructors: |leaf| and |branch|. 
}

> type Tree = Fix Tree_f
>
> leaf :: Tree
> leaf = In (Id Leaf)
>
> branch :: Int -> Tree -> Tree -> Tree
> branch v l r = In (Id (Branch v l r))

\review{
Sometimes it is easier for functions to work directly with a structure which
has fully annotated sub-structures. From now on we will call one single and
unannotated level of a recursive datatype a \emph{node}, nodes might contain
zero or more sub-structures. We create a type synonym |FixA1| that represents a
single node with fully annotated sub-structures.
}

> type FixA1 a f = f (FixA a f)

\noindent
\review{
We also introduce a type synonym |Fix1| similar to the |FixA1|, an alias for
the usage of a single node containing unannotated sub-structures. Note that
because the identity annotation does not store any additional information the
|Fix1| is isomorphic to |Fix|, but might sometimes be more convenient to use.
}

> type Fix1 f = f (Fix f)

\noindent
\review{
The annotated fixed points can be used to store arbitrary pieces of data at the
recursive positions of a recursive structure. To illustrate this using
something more interesting than the identity annotation we annotate a binary
tree with local modification times. In the following example every
sub-structure will be surrounded with an annotation that stores a Haskell
|LocalTime|, which might be filled in with the last time a sub-structure was
modified.
}

> data TimeAnn f a = TA LocalTime (f a)
> type TimedTree = FixA TimeAnn Tree_f

\end{subsection}

% -----------------------------------------------------------------------------

\begin{subsection}{Annotation associated functionality}

\review{
In the previous section we have shown how to store arbitrary pieces of
information at the recursive positions of a datatype. In this section we will
show how to associate functionality with these annotations. For every
annotation type we will describe how to obtain an annotation for a previously
unannotated node and how to get a node out of a fully annotated structure. The
process of creating new annotations we will call \emph{producing}, the process
of obtaining a node from an annotation we will call \emph{querying}. The
following two type signatures describe the produce and query actions.
}

> type Produce  a f m  =  f (  FixA   a f)  -> m (     FixA   a f)
> type Query    a f m  =       FixA   a f   -> m (f (  FixA   a f))

\noindent
\review{
As the type signature shows, a producer will take a node with fully annotated
sub-structures and introduces a new annotation for this node making it a fully
annotated structure again.  The function might run in some -- possibly monadic
-- context |m| when this is required for the annotation.  The type signature
for queries shows that it will take a fully annotated structure and will use
the annotation to give back an unannotated node with the sub-structures still
fully annotated.  Like the producer, this functions can also run in some
context |m|.
}

\review{
Two type classes are used to associate specific functionality to annotations.
For producers this class is called |AnnP|, for queries this class is called
|AnnQ|. Both type classes contain a single function with the type signature as
defined above. The first parameter of the type class, |a|, is the annotation
type, the second parameter, |f|, is the structure to annotate, the third, |m|,
is the context it may run in.
}

> class (Traversable f, AM m) => AnnQ a f m where
>   query :: Query a f m
>
> class (Traversable f, AM m) => AnnP a f m where
>   produce :: Produce a f m

\noindent
\review{
Making an annotation type an instance of these type classes means we can come
up with an annotation for a structure and we can get back a structure from an
annotation again.  Note that the |Traversable| and the |Monad| \footnote{In all
the examples that follow we assume that the occurrence of |Monad| in a type
context also assume the existence of an |Applicative| instance.  Although
this assumption is not strictly the case in Haskell it is valid in theory and
saves us some typing.} classes in the context are not strictly necessary super
classes here. These constraints only help to prune the contexts when using the
|AnnQ| and |AnnM| classes, because then |Traversable| and |Monad| are both
implied.
}

\review{
Now we can very easily make the identity annotation an instance of both the
|AnnP| and |AnnQ| type classes. We just unpack or pack the annotation and strip
off or introduce the |In| constructor.
}

> instance (Traversable f, AM m) => AnnQ Id f m where
>   query = return . unId . out
>
> instance (Traversable f, AM m) => AnnP Id f m where
>   produce = return . In . Id

\noindent
\review{
Although redundant in the general case, for possible future optimizations we
also introduce a type class for the modification of a sub-structure, called
|AnnM|. The |modify| function is used to apply a function over a single node
within a fully annotated structure.  There is a default implementation
available which is just the Kleisli composition (denoted by |<=<|) of the
query, the function, and the producer.
}

> type Modify a f m = (f (FixA a f) -> m (f (FixA a f))) -> (FixA a f -> m (FixA a f))
>
> class (AnnQ a f m, AnnP a f m) => AnnM a f m where
>   modify :: Modify a f m
>   modify f = produce <=< f <=< query

\noindent
\review{
For the identity annotation we just use the default implementation for |AnnM|.
}

> instance (Traversable f, AM m) => AnnM Id f m

\noindent
\review{
Now that we have defined both annotated fixed points and a type class to
associate functionality with annotations we can create two smart constructors
to simplify creating annotated binary trees manually.
}

> leafA :: AnnP a Tree_f m => m (TreeA a)
> leafA = produce Leaf
> 
> branchA :: AnnP a Tree_f m => Int -> TreeA a -> TreeA a -> m (TreeA a)
> branchA v l r = produce (Branch v l r)

\end{subsection}

% -----------------------------------------------------------------------------

\begin{subsection}{Debug annotation}

\review{
To more clearly demonstrate the usage of generic traversals over annotated
structures in the next section we first introduce the |Debug| annotation. In
contrast to the identity the debug annotation does have associated
functionality. It will print out a trace of every node that gets |produced| or
|queried|.
}

\review{
First we define the |Debug| data type that is just a |newtype| similar to the
identity annotation.
}

> newtype Debug f c = D { unD :: f c }

%if False

>   deriving Show

%endif

\noindent
\review{
Now we create a little helper function that can print out a predefined prefix
together with the some value and returns that same value again. Note that
function does not directly run in the |IO| monad, but in some monad |m| for
which there is a |MonadIO| instance, making it a bit more generally applicable.
}

> printer :: (MonadIO m, Show b) => String -> b -> m b
> printer s f =
>   do  liftIO (putStrLn (s ++ ": " ++ show f))
>       return f

\noindent
\review{
The |AnnQ| instance for the |Debug| annotation justs unpacks the constructor
and prints out the node that is queried, including the fully annotated
sub-structures.
}

> instance  (Traversable f, AMIO m, Show (FixA1 Debug f))
>       =>  AnnQ Debug f m
>    where  query = printer "query" . unD . out

\noindent
\review{
The same trick can be done for the dual instance |AnnP|. This function adds the
|In| and |D| constructors and also prints out the node that is being produced.
}

> instance  (Traversable f, AMIO m, Show (FixA1 Debug f))
>       =>  AnnP Debug f m
>    where  produce = fmap (In . D) . printer "produce"

\noindent
\review{
For the |AnnM| we use the default implementation.
}

> instance  (Traversable f, AMIO m, Show (FixA1 Debug f))
>       =>  AnnM Debug f m

\noindent
\review{
In order to get the above class instances to work properly we additionally need
a |Show| instance for our recursive structures. We represent the |In|
constructor by surrounding recursive structures with triangular brackets.
}

> instance Show ((a f) (FixA a f)) => Show (FixA a f) where
>   show f = "<" ++ show (out f) ++ ">"

\noindent
\review{
In the next chapters we will see how we can use the |Debug| annotation to print
out debug traces of generic traversals over annotated structures. Printing out
debug traces is just one example of what you can do with the annotation type
classes. In section \todo{ref to section} we will show how to use the same
annotation type classes to store and retrieve annotated structures to and from
disk.
}

\end{subsection}

\end{section}

