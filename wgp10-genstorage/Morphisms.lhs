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

\subsection{Catamorphism}

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
that instance as well, but since it does not add here, we omit it. GHC 6.12.1
and later can derive both |Foldable| and |Traversable| automatically.}
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

If we have an annotated tree, we can use the function. Let us assume that
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

Whe can now define |apoA|:

> apoA ::  (In ann f m, Monad m, Traversable f) =>
>          ApoCoalgebraA ann f s -> s -> m (FixA ann f)
> apoA psi = inA <=< mapM apoA' <=< return . psi
>   where  apoA' (Left   l)  =  apoA psi l
>          apoA' (Right  r)  =  return r

However, we unfortunately cannot directly use |apoA| to lift |insert|
to work on annotated binary search trees. The reason is that a modification
function such as |insert| both destructs and constructs a tree. If we want
to use an annotated binary search tree as seed for the apomorphism, we have
to destruct it in the coalgebra in order to pattern match -- but we cannot,
because destructing is associated with effects. Furthermore, we create new
leaves when inserting key-value pairs -- but again, we cannot, because
constructing new annotated values is associated with effects.

We therefore define a new recursion pattern for modifiers such as |insert|.
However, we need some additional utilities for this pattern that we
define first.\andres{Add pointers; rephrase.}

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

%if False

> {-

\subsection{Destructing with paramorphisms}

\andres[inline]{My first intuition is that we should explain the step from
catamorphisms to paramorphisms in more detail if we have the space.}

\emph{Paramorphisms} -- generalizations of the more widely known
\emph{catamorphisms} \cite{bananas} -- are implemented as recursive traversals that destruct a
recursive structure level-by-level to some result value. The traversal function
is parametrized by an \emph{algebra}, a description of a recursive operation.
An algebra is a function that produces a result function for a single level.\andres{Rephrase.} As
input, an algebra takes a functor with at the recursive positions both the
results values of the destruction of previous levels and the original sub
structures at those positions.

%if False

> type a :*: b = (a, b)
> -- type a :+: b = Either a b

%endif

> data AlgA  (a  ::  (  *  -> *) -> * -> *  )
>            (f  ::     *  -> *             )
>            (r  ::     *                   ) where
>   Psi :: (f (r :*: FixA a f) -> r)  -> AlgA a f r

The function |paraA| then takes an algebra that computes a result value from a
one-level structure and uses the algebra to recursively destruct an entire
recursive structure:

> paraA :: (Out a f m, Traversable f) => AlgA a f r -> FixA a f -> m r
> paraA (Psi p)  =    return . p
>                <=<  mapM (group (paraA (Psi p)))
>                <=<  outA
>   where group f c = liftM (,c) (f c)

\todo{literally explain this code?}\andres{Yes, a little bit more, but
it becomes easier if we switch to cata.}
This paramorphism works on annotated structures; before computing the
subresults and before applying the algebra, it first unwraps the annotation for
the top-level node using the |outA| function.

We can write algebras of paramorphisms as if there are no annotations, and
then use the |paraA| function to lift such algebras to work on annotated
structures. Algebras that do not make use of a particular annotation must
be polymorphic in the annotation type:

> type Alg f r = forall a. AlgA a f r

As an example, let us reimplement the |lookup1| function for binary search
trees as an algebra:

> lookupAlg' :: Ord k => k -> Alg (TreeF k v) (Maybe v)
> lookupAlg' k = Psi $ \t ->
>   case t of
>     Leaf            ->  Nothing
>     Branch c w l r  ->  case k `compare` c of
>                           LT  ->  fst l
>                           EQ  ->  Just w
>                           GT  ->  fst r

Note the difference between |lookupAlg| and the |lookup1| function
from Section~\ref{sec:fixpoints}: the original function directly uses recursion
the find the value, the algebra reuses the subresults stored at the recursive
positions of the input node.\andres{Even better if we have already given
an algebra before. Then we can analyze the overhead introduced by
annotations, which is nearly none.}

We can \emph{run} the algebra by supplying it to~|paraA|:

> lookup'  ::  (Ord k, Out a (TreeF k v) m)
>         =>  k -> TreeA a k v -> m (Maybe v)
> lookup' k = paraA (lookupAlg' k)

The algebra can be annotation-agnostic, because it abstracts from
recursion and outsources recursion to the |paraA| recursion pattern.

Another interesting example of a paramorphism is the following: we use
a custom function to map all values in the binary tree to elements of
a monoid. We then use the monoid operator |(<>)|\footnote{|(<>)| is written as
@mappend@ in Haskell} to combine all these values into a single result.
The algebra |foldAlg| is defined as follows:

> foldAlg :: Monoid m => (v -> m) -> Alg (TreeF k v) m
> foldAlg f = Psi $ \t -> case t of
>   Leaf            ->  mempty
>   Branch _ v l r  ->  fst l <> f v <> fst r

The algebra |foldAlg| can be instantiated to a large number of useful functions.
One example is the function |toList| that flattens a binary search tree using
an inorder traversal:

> toList :: Out a (TreeF k v) m => TreeA a k v -> m [v]
> toList = paraA (foldAlg (\x -> [x]))

Once again, note that we have written the code in exactly the same way as
we would have without annotations. The resulting function |toList| is fully
polymorphic in the annotation type |a|.

We test the |toList| function by applying it to the result of the list in
Section~\ref{sec:debug}. Because that value makes use of the debug annotatoin,
we now also see a full debug trace of the unwrap steps performed during the
traversal:
\begin{verbatim}
ghci> toList it :: IO [Int]
("out",Branch 3 9 () ())
("out",Branch 1 1 () ())
("out",Leaf)
("out",Leaf)
("out",Branch 4 16 () ())
("out",Branch 7 49 () ())
("out",Leaf)
("out",Leaf)
("out",Leaf)
[1,9,49,16]
\end{verbatim}

\todo{we might want to change this to cata after all}
Note that both the |lookupAlg| and the |foldAlg| algebras only use the first
component of the tuple that is supplied to the algebra by the paramorphism.
Because only the recursive results are used and not the original substructures
these algebras actually are \emph{catamorphisms}, a special case of
paramorphisms. In Section~\ref{sec:modification} we discuss a morphism that
really needs to access the original substructures in its algebra.

The algebras are written in pure style, no annotations appear in the algebra
and no monadic context is used. Using the annotated paramorphism function
|paraA|, we
interpreted the algebras in monadic context and apply the |outA| function to
the annotated structure where needed. We can also specialize |paraA|
function to work with the identity annotation in the identity monad. We then
obtain pure operations on unannotated structures:

> para :: Traversable f => AlgA Id1 f r -> Fix f -> r
> para p = runIdentity . paraA p . runIdentity . fullyIn

By abstracting away from recursion we can reuse a single operation in different
contexts, both for annotated and unannotated recursive structures.

\subsection{Constructing with apomorphisms}

Where paramorphisms are used to destruct recursive datatypes into a result
value, \emph{apomorphisms} are used to construct recursive datatypes from a
seed value. Apomorphisms are generalizations of the more widely known
\emph{anamorphisms}. Apomorphisms build recursive structures from an initial
seed value and a \emph{coalebgra}. A coalebgra takes a seed value and produces
a single node with at the recursive positions either a new seed value or
an existing recursive structure:

> data CoalgA  (a ::  (  *  -> *) -> * -> *  )
>              (f ::     *  -> *             )
>              (s ::     *                   ) where
>   Phi :: (s -> f (Either s (FixBotA a f))) -> CoalgA a f s

As for paramorphisms, we define a type synonym for coalgebras that do not
make use of a particular annotation:

> type Coalg s f = forall a. CoalgA a f s

The apomorphism function~|apoA| takes a coalgebra an applies it repeatedly
in order to produce an entire recursive structure. Wherever the coalgebra
produces a new seed value inside a node, the |apoA| function recursively
continues the construction.

> apoA' :: (Traversable f, In a f m) => CoalgA a f s -> s -> m (FixA a f)
> apoA' (Phi p)  =    inA
>               <=<  mapM (apoA' (Phi p) `either` topIn) . p

As an example, we define a coalgebra |fromSortedListCoalg| that creates
a binary search tree from a sorted list of key-value pairs. If the input is an
empty list, we produce a |Leaf|. A leaf has no arguments, hence the
construction stops. If the input list is not empty, we generate a
|Branch|. The middle element is stored in the node, the remaining parts
of the list are used as new seed values for the left and right subtrees.

> fromSortedListCoalg :: Coalg [(k, v)] (TreeF k v)
> fromSortedListCoalg = Phi $ \t ->
>   case t of
>     []  ->  Leaf
>     xs  ->  let  l        =  take ((length xs `div` 2) - 1) xs
>                  (k,v):r  =  drop (length l               ) xs
>             in Branch k v (Left l) (Left r)

The function~|fromList| runs the coalgebra by passing it to the recursion
pattern~|apoA|. In addition, we first sort the input list on the key values
to establish the precondition of~|fromSortedListCoalg|.

> fromList'  ::  (In a (TreeF k v) m, Ord k)
>            =>  [(k, v)] -> m (FixA a (TreeF k v))
> fromList'  =   apoA' fromSortedListCoalg
>            .   sortBy (comparing fst)

Once again, the functions are written in a pure style. However, we can
choose to run |formList| in an |IO| context for the |Debug| annotation
and obtain a debug trace that shows how the example tree is created from
the input list:
\begin{verbatim}
ghci> let squares = [(1,1),(3,9),(4,16),(7,49)]
ghci> fromList squares :: IO (TreeD Int Int)
("in",Leaf)
("in",Leaf)
("in",Branch 1 1 () ())
("in",Leaf)
("in",Leaf)
("in",Leaf)
("in",Branch 9 81 () ())
("in",Branch 4 16 () ())
("in",Branch 3 9 () ())
{D (Branch 3 9 {D (Branch 1 1 {D Leaf} {D Leaf})}
{D (Branch 4 16 {D Leaf} {D (Branch 7 49 {D Leaf}
{D Leaf})})})}
\end{verbatim}

As for the paramorphism, we can also create a pure, unannotated variant of
the apomorphism by using the identity annotation in the identity monad:

> apo :: Traversable f => CoalgA Id1 f s -> s -> Fix f
> apo phi = runIdentity . fullyOut . runIdentity . apoA' phi

> -}

%endif

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
> insertAlg k v (Branch n x l r)
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
