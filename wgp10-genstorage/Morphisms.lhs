%if False

> {-# LANGUAGE
>     ScopedTypeVariables
>   , KindSignatures
>   , FlexibleContexts
>   , FlexibleInstances
>   , TupleSections
>   , GADTs
>   , RankNTypes
>   , MultiParamTypeClasses
>   , TypeOperators
>   , TupleSections
>   #-}
> module Morphisms where

> import Data.Ord
> import Data.List hiding (group)
> import Data.Monoid
> import Prelude hiding (mapM)
> import Control.Monad.Identity (Identity(..))
> import Control.Monad hiding (mapM, (<=<))
> import Data.Foldable
> import Data.Traversable
> import Fixpoints hiding (Algebra, cata, lookup, Coalgebra, fromList, insertAlg)

%endif

\section{Annotated recursion patterns}
\label{sec:patterns}

Writing operations on annotated datatypes directly is inconvenient.  The |inA|
and |outA| methods have monadic result type, forcing us to use monadic style
all over the place. Furthermore, if we want to write code that is generic over
all annotations, we cannot use pattern matching, because we do not know how the
annotated terms look like.

In this section, we therefore have another look at catamorphisms, anamorphisms
and apomorphisms. We discuss how these recursion patterns have to be adapted so
that they work with annotated structures.

Using recursion patterns, we can avoid the problems that directly defined
operations on annotated structures have. As we will see, in many cases we can
reuse the original algebras, written in a pure, annotation-agnostic way. By
plugging them into the new patterns, we can still run them in a framework that
performs effectful operations behind the scenes.

\subsection{Catamorphism}\label{sec:catamorphisms}

Recall the definition of a catamorphism from Section~\ref{sec:simplerecpat}:

> type Algebra f r = f r -> r
>
> cata :: Functor f => Algebra f r -> Fix f -> r
> cata phi = phi . fmap (cata phi) . out

In order to move to the annotate setting, we replace |out|
by |outA|; as a consequence, everything becomes monadic,
so we replace function composition by Kleisli composition;
finally, we replace |fmap| by |mapM|:

> cataA ::  (Out ann f m, Monad m, Traversable f) =>
>           Algebra f r -> FixA ann f -> m r
> cataA phi = return . phi <=< mapM (cataA phi) <=< outA

Haskell's |Traversable| type class replaces the |Functor| constraint -- it
contains the |mapM| method. Note that we use the same |Algebra| type as before,
and assume pure algebras written in an annotation-agnostic way -- exactly as we
want.

Before we can use |cataA| on an actual datatype such as binary search trees,
we have to give a |Traversable| instance for the pattern functor:\footnote{%
Haskell's |Traversable| has |Foldable| as superclass, so we have to define
that instance as well, but since its functionality is not used here, we omit it.
GHC 6.12.1 and later can derive both |Foldable| and |Traversable| automatically.}
%if False

> instance Foldable (TreeF k v) where
>   foldMap _ Leaf              = mempty
>   foldMap f (Branch _ _ l r)  = f l <> f r

%endif

> instance Traversable (TreeF k v) where
>   mapM _ Leaf              = return Leaf
>   mapM f (Branch k v l r)  = liftM2 (Branch k v) (f l) (f r)

As before, we obtain an actual lookup function by passing the algebra to~|cataA|:

> lookup k = cataA (lookupAlg k)

We can use |lookup| once we have an annotated tree. Let us assume that
@it@ is bound to the result of evaluating |myTree_a| using the modification
time annotation. The following returns the expected result, but now in the |IO| monad:
\begin{verbatim}
ghci> lookup 4 it
Just 16
\end{verbatim}

However, if we assume that @it@ is bound to the result of evaluating |myTree_a|
in the debug annotation, the call to |lookup| reveals a problem:
\begin{verbatim}
ghci> lookup 4 it
("out",Branch 3 9 () ())
("out",Branch 1 1 () ())
("out",Leaf)
("out",Leaf)
("out",Branch 4 16 () ())
("out",Branch 7 49 () ())
("out",Leaf)
("out",Leaf)
("out",Leaf)
Just 16
\end{verbatim}
The function produces a result and a trace as expected. However, the trace
reveals that the \emph{entire} tree is traversed, not just the path to the
|Branch| containing the key~|4|. The culprit is the strictness of~|IO|, that
propagates to the whole operation now that |IO| is used behind the scenes.
We defer the discussion of this problem until Section~\ref{sec:laziness}.

We can use the annotated catamorphism also to remove all annotations from
a recursive structure, removing all layers of annotations and performing
the associated effects:

> fullyOutA ::  (Out ann f m, Monad m, Traversable f) =>
>               FixA ann f -> m (Fix f)
> fullyOutA = cataA In

\subsection{Anamorphism}

For anamorphisms, the situation is very similar as for catamorphisms. We
define an annotated variant of |ana|, called |anaA|, by lifting everything
systematically to the annotated monadic setting:

> type Coalgebra f s = s -> f s
>
> anaA ::  (In ann f m, Monad m, Traversable f) =>
>          Coalgebra f s -> s -> m (FixA ann f)
> anaA psi = inA <=< mapM (anaA psi) <=< return . psi

Note that the |Coalgebra| type synonym is unchanged and just repeated here
for convenience.

We can now produce annotated values more conveniently. Instead of
using a monadic construction such as |myTree_a|, we can resort to |fromList|:

> fromList xs = anaA  fromSortedListAlg
>                     (sortBy (comparing fst) xs)

The definition

> myTree_a' :: In ann (TreeF Int Int) m => m (TreeA ann Int Int)
> myTree_a' = fromList [(1,1),(3,9),(4,16),(7,49)]

is equivalent to the old |myTree_a|.

With the catamorphism we can remove all annotations from a structure --
with the anamorphism we can completely annotate a recursive structure:

> fullyInA ::  (In a f m, Monad m, Traversable f) =>
>              Fix f -> m (FixA a f)
> fullyInA = anaA out

\subsection{Apomorphism}\label{sec:apomorphisms}

The situation for the apomorphism is a bit different, because
the fixed point combinator occurs in the type of coalgebras:

> type ApoCoalgebra f s = s -> f (Either s (Fix f))

In a first step, we are changing the coalgebra type to use |FixA|
instead:

> type ApoCoalgebraA ann f s = s -> f (Either s (FixA ann f))

We can now define |apoA|:

> apoA ::  (In ann f m, Monad m, Traversable f) =>
>          ApoCoalgebraA ann f s -> s -> m (FixA ann f)
> apoA psi = inA <=< mapM apoA' <=< return . psi
>   where  apoA' (Left   l)  =  apoA psi l
>          apoA' (Right  r)  =  return r

Unfortunately, we cannot directly use |apoA| to lift |insert|
to work on annotated binary search trees. The reason is that a modification
function such as |insert| both destructs and constructs a tree. If we want
to use an annotated binary search tree as seed for the apomorphism, we have
to destruct it in the coalgebra in order to pattern match -- but we cannot,
because destructing is associated with effects. Furthermore, we create new
leaves when inserting key-value pairs -- but again, we cannot, because
constructing new annotated values is associated with effects.

We therefore define a new recursion pattern for modifiers such as |insert|
in Section~\ref{sec:modification}. However, we need some preparation for
that, so we first look at partially annotated structures.

\subsection{Partially annotated structures}

Let us capture the idea of building some layers of a recursive structure
in a pure, annotation-agnostic way, while still being able to reuse parts
of an annotated structure that we have available already.

To this end, we introduce an \emph{annotation transformer} called |Partial|.
Given an annotation |ann|, we can either choose to use a complete annotated
subtree, or create a new unannotated layer:

> data Partial ann f a  =  New  (f a)
>                       |  Old  (FixA ann f)

We define an abbreviation for partially annotated structures:

> type FixPartialA ann f = FixA (Partial ann) f

Using the function |topIn|, we can complete the missing annotations at the
top of a partially annotated structure:

> topIn ::  (In ann f m, Monad m, Traversable f) =>
>           FixPartialA ann f -> m (FixA ann f)
> topIn = topIn' . out
>   where  topIn' (New  x) = (inA <=< mapM topIn) x
>          topIn' (Old  x) = return x

\subsection{Modification functions}
\label{sec:modification}

By employing the partially annotated structures, we can now introduce
a variant of the apomorphism that modifies a given structure.

As a preparation, we define a type class~|OutIn| that combines the functionality
of the |Out| and |In| classes: using the class method |outInA|,
an annotated node is unwrapped, modified, and finally re-wrapped:

> class (Out ann f m, In ann f m) => OutIn ann f m where
>   outInA  ::   (  f  (FixA ann f) -> m (   f  (FixA ann f)))
>           ->         (FixA ann f) -> m        (FixA ann f)
>   outInA f = inA <=< f <=< outA

We require both |Out| and |In| as superclasses of |OutIn|. Given the |outA| and
|inA| methods, we supply a default implementation for |outInA|.
For all the annotations we have been using so far, the default implementation
is sufficient:

> instance  OutIn Id1 f Identity
> instance  OutIn ModTime f IO
> instance  (Functor f, Show (f ())) => OutIn Debug f IO

For some annotation types (such as the |Heap|
annotation we use in Section~\ref{sec:storage}), we can give an improved direct
definition.

We can now define a variant of the apomorphisms for modification functions
that is different from the normal apomorphism in the following ways:
\begin{itemize}
\item the type of the seed is restricted to be a value of the recursive
  structure itself;
\item instead of stopping the recursion by returning a fully annotated tree,
  we allow to stop with a partially annotated tree.
\end{itemize}
We give both the old and the new coalgebra type for comparison:

> type ApoCoalgebraA'      ann f s  =
>   s               ->  f (Either s             (FixA ann f))
>
> type EndoApoCoalgebraA   ann f    =
>   f (FixA ann f)  ->  f (Either (FixA ann f)  (FixPartialA ann f))

The associated recursion looks as follows:

> endoApoA ::  (OutIn ann f m, Monad m, Traversable f) =>
>              EndoApoCoalgebraA ann f -> FixA ann f -> m (FixA ann f)
> endoApoA psi = outInA $ mapM endoApoA' . psi
>   where  endoApoA' (Left   l)  =  endoApoA psi l
>          endoApoA' (Right  r)  =  topIn r

Compared to the regular apomorphism, we use |outInA| because we now
work with the same source and target structure, and we use |topIn|
to create the missing annotations when we stop the recursion.

When defining a coalgebra for use with |endoApoA|, we now effectively
have the choice between the following three actions per recursive
position: 
\begin{itemize}
\item We can produce a new seed to drive the next recursive step,
  by selecting the left part of the sum type in the result of the
  coalgebra. We define a helper function with the more meaningful
  name~|next| for this choice:

> next :: FixA ann f -> Either (FixA ann f) (FixPartialA ann f)
> next = Left

\item We can reuse a fully annotated part of the input as output,
  stopping the recursion at this point.
  We define the helper function |stop| for this purpose:

> stop :: FixA ann f -> Either (FixA ann f) (FixPartialA ann f)
> stop = Right . In . Old

\item Finally, we can create one or more layers of new nodes, by using
the helper function |make|, which takes a partially annotated
structure as its argument:

> make :: f (FixPartialA ann f) -> Either (FixA ann f) (FixPartialA ann f)
> make = Right . In . New

\end{itemize}

We can now return to our example, the |insert| function on binary search
trees. Unfortunately, we cannot quite reuse the original coalgebra we have
given in Section~\ref{sec:simplerecpat}. We have to be more explicit about
where we reuse old parts the tree, and where we create new parts of the
tree:

> insertAlg ::  Ord k =>
>               k -> v -> EndoApoCoalgebraA ann (TreeF k v)
> insertAlg k v Leaf =
>   Branch k v (make Leaf) (make Leaf)
> insertAlg k v (Branch n x l r) =
>       case k `compare` n of
>         LT  -> Branch n x (next  l) (stop  r)
>         _   -> Branch n x (stop  l) (next  r)

The differences are relatively minor:
We can still define |insertAlg| as a pure function, and annotation-agnostic.
We no longer have to pattern match on parts of the recursive structure,
because we use the endo-apomorphism now. And we can still get the original
behaviour back, by specializing to the identity annotation and the identity
monad.

We run |insertAlg| by passing it to |endoApoA|:

> insert k v = endoApoA (insertAlg k v)

\subsection{Summary}

In this and the previous section we have shown a framework for generically
annotating recursive datatypes. Using an annotated fixed point combinator we
are able to store custom markers (containing potentially custom information) at
the recursive positions of functional data structures. We associate extra
functionality -- potentially with effects -- with the creation, removal and
modification of annotated recursive structures.

By defining algebras for specific recursion patterns, we can define functions
in a pure style, without having to worry about annotations or monadic contexts.
We have shown a number of frequently occurring patterns, for consumers,
for producers, and for modifiers. More patterns can be defined in a similar style
if desired. For example, we can define an endo-paramorphism that is dual to the
endo-apomorphism.

We have shown that several operations on binary search trees can be expressed
using such patterns. In fact, we have built a library that replicates most of
a finite map data structure based on binary search trees in our annotated framework,
yielding a data structure that can be flexibly used with several annotations.
