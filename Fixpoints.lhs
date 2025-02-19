%if False

> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE
>     KindSignatures
>   , UndecidableInstances
>   , TypeOperators
>   , MultiParamTypeClasses
>   , FlexibleInstances
>   , FlexibleContexts
>   #-}
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

\begin{chapter}{Annotated fixed points}
\label{chap:fixpoints}

In Haskell datatypes can be recursive. Recursive datatypes have one or more
references back to the original datatype in the fields of their constructors.
Recursion in datatypes allows for building data structure that store multiple
elements. Examples of recursive data structures are linked lists, binary trees, and
sequences. This chapter explains a technique that allows us to store
annotations inside the recursive positions of recursive datatypes. These
annotations are used to associate custom functionality to the construction
and destruction of recursive data structures.

% -----------------------------------------------------------------------------

\begin{section}{Fixed points}

Most container datatypes in Haskell are written down with explicit recursion.
An example of a container type using explicit recursion is the binary tree
datatype. This binary tree stores both a value and two explicit sub-trees in
the branch constructor, empty trees are indicated by a leaf.

> data Tree_1 = Leaf_1 | Branch_1 Int Tree_1 Tree_1

In figure \ref{fig:binarytree} we see an example of binary tree with four values.

\begin{figure}[hp]
\label{fig:binarytree}
\begin{center}
\includegraphics[scale=0.8]{./binarytree.pdf}
\end{center}
\caption{An example of a binary tree.}
\end{figure}

To gain more control over the recursive positions of the datatype we can
parametrize the binary tree with an additional type parameter used at the
recursive positions. Not the tree datatype itself, but the users of the
datatype may now decide what values to store as sub-trees. We call this new
datatype |Tree_f|, the tree functor.

> data Tree_f f = Leaf | Branch Int f f
>   deriving Show

To get back a binary tree that is isomorphic to our original binary tree,  we can use an explicit
fixed point combinator at the type level. 
This combinator, conventionally
called |Fix_1|, takes a type constructor of kind |* -> *| and parametrizes this
type with its own fixed point.

> newtype Fix_1 (f :: * -> *) = In_1 { out_1 :: f (Fix_1 f) }

By applying the fixed point combinator to the tree functor we get a back a true
binary tree, with real sub-trees at the recursive positions. The sub-trees are wrapped in an |In| constructor.

> type Tree_2 = Fix_1 Tree_f

We call datatypes that abstract away from recursion using an additional
type parameter \emph{open recursive datatypes}.

In figure \ref{fig:binarytreefix} we see an example of a binary tree that uses
a fixed point combinator at the recursive positions.

\begin{figure}[tp]
\label{fig:binarytreefix}
\begin{center}
\includegraphics[scale=0.8]{./binarytree-fix.pdf}
\end{center}
\caption{An example of a binary tree written as an open recursive datatype. The
fixed point combinator is used to tie the knots.}
\end{figure}

To make it easier to deal with the recursive structure of the binary tree we
can make the |TreeF| type an instance of Haskell's |Functor| type class. We now call |TreeF| the \emph{tree functor}. The
functorial |fmap| takes the input function and lifts it to be applied against
the sub-structures of the binary tree.

> instance Functor Tree_f where
>   fmap _  Leaf            = Leaf
>   fmap f  (Branch v l r)  = Branch v (f l) (f r)

Besides |Functor| Haskell has two additional type classes that help with
generic traversals over container datatypes. These are the |Foldable| and
|Traversable| type classes\footnote{Note that these type class instances are
simple and mechanically derivable. The GHC Haskell compiler version 6.12.1
and above is able to derive the instances for |Functor|, |Foldable| and
|Traversable| for you automatically.}. The |Foldable| type class allows us to
reduce an entire structure into a single value using some |Monoid| operation. 

> instance Foldable Tree_f where
>   foldMap _  Leaf            = mempty
>   foldMap f  (Branch _ l r)  = f l `mappend` f r

The |Traversable| type class, which requires |Foldable| as its super class,
allows a generic traversal over a structure while performing an action for each
element. The actions performed are |Applicative| or sometimes monadic
computations. The |Traversable| instance for our binary tree example is a
straightforward preorder traversal. The actions are written down using idiom
brackets. Mcbride and Paterson \cite{idioms} show how idiom brackets can be used for effecful
applicative programming.

> instance Traversable Tree_f where
>   traverse _  Leaf            = (| Leaf |)
>   traverse f  (Branch v l r)  = (| (Branch v) (f l) (f r) |)

Having instances of the |Traversable| class around is useful, because it
allows us to use the generic version of the Prelude's |mapM| function. This
function enables us to |fmap| a monadic action over a structure and transpose
the result.

> mapM1 :: (Traversable f, AM m) => (a -> m b) -> f a -> m (f b)

The |mapM1| function can be used to perform a lightweight form of generic
programming. The function allows us to apply computations against the recursive
values of any data structure that has a |Traversable| instance.

%if False

> mapM1 = undefined

%endif

\end{section}

% -----------------------------------------------------------------------------

\begin{section}{Annotations}
\label{sec:fixann}

In the previous section we worked out some basic building blocks that can be
useful when working with container datatypes which are explicitly parametrized
with the recursive structures.  But why is it useful to abstract away
from recursion in the first place? This section shows how we can store
additional information at the recursive positions of open recursive datatypes
using an annotated fixed point combinator.

First we introduce a new fixed point combinator that optionally stores an
annotation over a container datatype instead of a datatype directly. This type
level fixed point combinator is called |FixA|. \footnote{ The |FixA| |newtype|
might feel redundant at first sight, because we could as well just parametrize
the original |Fix| with an annotated structure |(a f)|, yielding the same
result.  From the usage of the |FixA| it has become clear that expressing the
more specific fixed point |Fix| in terms of the more general |FixA| helps us to
more easily reuse functionality later on.} Throughout this document the
\emph{alpha} postfix is used to indicate that a type or a function is
annotation aware. The |FixA| combinator has two constructors, one that stores
an annotation over a structure |f| and one that stores a plain unannotated |f|,
with possibly annotated sub-structures.

> data FixA a f =
>      InA  { outa  :: (a  f)  (FixA a f) }
>   |  InF  { outf  ::     f   (FixA a f) }

Note the kind of the annotation variable |a|, the annotation is applied to the
original container type |f| which has kind |* -> *|. Because the annotation
itself applied to the container type |f| needs to have the same kind as |f|,
the variable |a| has kind |(* -> *) -> (* -> *)|.

It is now easy to define a fully annotated binary tree by applying the
annotated fixed point combinator to the tree functor.

> type TreeA a = FixA a Tree_f

In figure \ref{fig:binarytree-ann} we see an example of binary tree that uses
an annotated fixed point to tie the recursive knot.

\begin{figure}[tp]
\label{fig:binarytree-ann}
\begin{center}
\includegraphics[scale=0.8]{./binarytree-ann.pdf}
\end{center}
\caption{An example of an annotated binary tree.}
\end{figure}

We now introduce the identity annotation, called |Id|, that stores no
additional information but just encapsulates the underlying container type.

> newtype Id f a = Id1 { unId :: f a }

%if False

>  deriving Show

%endif

The identity annotation can be used to get back the regular fixed point
combinator defined in the previous section by plugging it into a |FixA|.
Because the identity annotation stores no additional information we call a |Fix
f| structure an unannotated or plain structure.

> type Fix f = FixA Id f

Working with a fully annotated structure using the |FixA| combinator or working
with a plain structure using the |Fix| combinator both require all
sub-structures to be surrounded by an additional |In| constructor. To make the
usage of plain binary trees more easy we create a |Tree| type synonym and two
smart constructors: |leaf| and |branch|. 

> type Tree = Fix Tree_f
>
> leaf :: Tree
> leaf = InA (Id1 Leaf)
>
> branch :: Int -> Tree -> Tree -> Tree
> branch v l r = InA (Id1 (Branch v l r))

The annotated fixed points can be used to store arbitrary pieces of data at the
recursive positions of a recursive structure. To illustrate this using
something more interesting than the identity annotation we annotate a binary
tree with local modification times. In the following example every
sub-structure is surrounded with an annotation that stores a Haskell
|LocalTime|, which can be filled in with the last time a sub-structure was
modified.

> data TimeAnn f a = TA LocalTime (f a)
> type TimedTree = FixA TimeAnn Tree_f

\end{section}

% -----------------------------------------------------------------------------

\begin{section}{Annotation associated functionality}
\label{sec:annfun}

In the previous section we have shown how to store arbitrary pieces of
information at the recursive positions of a datatype. In this section we
show how to associate functionality with these annotations. For every
annotation type we describe how to obtain an annotation for a previously
unannotated node and how to get a node out of a fully annotated structure.
We create one type synomym for the process of putting a structure inside an
annotation an one for getting a structure out of an annotation. We call an |InA|
function a producer function and a |Out| function a query function.

> type In   a f m  =  f (  FixA   a f)  -> m (     FixA   a f)
> type Out  a f m  =       FixA   a f   -> m (f (  FixA   a f))

As the type signature shows, a producer takes a node with fully annotated
sub-structures and introduces a new annotation for this node making it a fully
annotated structure again.  The function can run in some -- possibly monadic
-- context |m| when this is required for the annotation.  The type signature
for queries shows that it takes a fully annotated structure and uses
the annotation to give back an unannotated node with the sub-structures still
fully annotated.  Like the producer, this functions can also run in some
context |m|.

Two type classes are used to associate specific functionality to annotations.
For producers this class is called |AnnI|, for queries this class is called
|AnnO|. Both type classes contain a single function with the type signature as
defined above. The first parameter of the type class, |a|, is the annotation
type, the second parameter, |f|, is the structure to annotate, the third, |m|,
is the context it may run in.

> class (Traversable f, AM m) => AnnI a f m where
>   annI :: In a f m
>
> class (Traversable f, AM m) => AnnO a f m where
>   annO :: Out a f m

Making an annotation type an instance of these type classes means we can come
up with an annotation for a structure and we can get back a structure from an
annotation again.  Note that the |Traversable| and the |Monad| \footnote{In all
the examples that follow we assume that the occurrence of |Monad| in a type
context also assume the existence of an |Applicative| instance.  Although
this assumption is not strictly the case in Haskell it is valid in theory and
saves us some typing.} classes in the context are not strictly necessary super
classes here. These constraints only help to prune the contexts when using the
|AnnO| and |AnnIO| classes, because then |Traversable| and |Monad| are both
implied.

Now we can make the identity annotation an instance of both the |AnnI| and
|AnnO| type classes. We just unpack or pack the annotation and strip off or
introduce the |InA| constructor. For the |InF| case we do not need to do any
work.

> instance (Traversable f, AM m) => AnnO Id f m where
>  annO (InA (Id1  f))  = return f
>  annO (InF       f )  = return f

> instance (Traversable f, AM m) => AnnI Id f m where
>   annI = return . InA . Id1

Although redundant in the general case, for possible future optimizations we
also introduce a type class for the modification of a sub-structure, called
|AnnIO|. The |annIO| function is used to apply a function over a single node
within a fully annotated structure.  There is a default implementation
available which is just the Kleisli composition (denoted by |<=<|) of the
query, the function, and the producer.

> type InOut a f m  =   (f (  FixA a f)  -> m (f (  FixA a f)))
>                   ->  (     FixA a f   -> m (     FixA a f))

> class (AnnO a f m, AnnI a f m) => AnnIO a f m where
>   annIO :: InOut a f m
>   annIO f = annI <=< f <=< annO

For the identity annotation we just use the default implementation for |AnnIO|.

> instance (Traversable f, AM m) => AnnIO Id f m

Now that we have defined both annotated fixed points and a type class to
associate functionality with annotations we can create two smart constructors
to simplify creating annotated binary trees manually.

> leafA :: AnnI a Tree_f m => m (TreeA a)
> leafA = annI Leaf
> 
> branchA :: AnnI a Tree_f m => Int -> TreeA a -> TreeA a -> m (TreeA a)
> branchA v l r = annI (Branch v l r)

\end{section}

% -----------------------------------------------------------------------------

\begin{section}{Multi-level annotation}

In the previous chapter we have introduced how to wrap and unwrap a single
level of a fully annotated structure. In this chapter we introduce two
additional functions that allows us to perform multi-level wrapping and
unwrapping of annotations.

First we define the function |fullyOut| that recursively unwraps all
annotations from the top of an annotated strucuture. Only unwrapping
annotations at the top means this function \emph{assumes} that once it finds an
unannotated node the functions stops. An unannotated node is indicated by the
use of the |InF| constructor in the fixed point.

> fullyOut :: (Traversable f, AnnO a f m) => FixA a f -> m (FixA a f)
> fullyOut (InA a)  = annO (InA a) >>= fmap InF . traverse fullyOut
> fullyOut (InF f)  = return (InF f)

The dual function |fullyIn| performs the inverse process of |fullyOut|, it
recursively annotated the top a (partially) unannotated structure. It
recursively wraps all unannotated nodes in an annotation, when it finds a node
that is already annotated it stops.

> fullyIn :: (Traversable f, AnnI a f m) => FixA a f -> m (FixA a f)
> fullyIn (InF f)  = traverse fullyIn f >>= annI 
> fullyIn (InA a)  = return (InA a)

When we assume the invariant that all the sub trees of an unannotated node do
not contain any annotations, |fullOut| ensures the entire structure 
is unannotated. When we assume the invariant that all sub trees of an
annotated node are fully annotated, |fullyIn| ensures the entire structure
is annotated.

In the chapter \ref{chap:morphisms} about generic annotated traversals we
see that the |fullyIn| function simplifies writing algebras for both
endomorphic paramorphisms and endomorphic apomorphisms.

\end{section}

% -----------------------------------------------------------------------------

\begin{section}{Debug annotation}

To more clearly demonstrate the usage of generic traversals over annotated
structures in the next section we first introduce the |Debug| annotation. In
contrast to the identity the debug annotation does have associated
functionality. It prints out a trace of every node that gets |produced| or
|queried|.

First we define the |Debug| datatype that is just a |newtype| similar to the
identity annotation. No additional information is stored, the |newtype| is only
used to associate specific actions to this annotation.

> newtype Debug f c = D { unD :: f c }

%if False

>   deriving Show

%endif

Now we create a little helper function that can print out a predefined prefix
together with the some value and returns that same value again. Note that
function does not directly run in the |IO| monad, but in some monad |m| for
which there is a |MonadIO| instance, making it a bit more generally applicable.

> printer :: (MonadIO m, Show b) => String -> b -> m b
> printer s f =
>   do  liftIO (putStrLn (s ++ ": " ++ show f))
>       return f

The |AnnO| instance for the |Debug| annotation justs unpacks the constructors
and prints out the node that is queried, including the fully annotated
sub-structures.

> instance  (Traversable f, AMIO m, Show (f (FixA Debug f)))
>       =>  AnnO Debug f m
>    where  annO (InA (D  f)  ) = printer "annO" f
>           annO (InF     f   ) = printer "annO" f

The same trick can be done for the dual instance |AnnI|. This function adds the
|InA| and |D| constructors and also prints out the node that is being produced.

> instance  (Traversable f, AMIO m, Show (f (FixA Debug f)))
>       =>  AnnI Debug f m
>    where  annI = fmap (InA . D) . printer "annI"

For the |AnnIO| we use the default implementation.

> instance  (Traversable f, AMIO m, Show (f (FixA Debug f)))
>       =>  AnnIO Debug f m

In order to get the above class instances to work properly we additionally need
a |Show| instance for our recursive structures. We represent the |InA|
constructor by surrounding recursive structures with triangular brackets.

> instance Show ((a f) (FixA a f)) => Show (FixA a f) where
>   show f = "<" ++ show (outa f) ++ ">"

In the next chapters we see how we can use the |Debug| annotation to print
out debug traces of generic traversals over annotated structures. Printing out
debug traces is just one example of what you can do with the annotation type
classes. In chapter \ref{chap:storage} we show how to use the same
annotation type classes to store and retrieve annotated structures to and from
disk.

\end{section}

\end{chapter}

