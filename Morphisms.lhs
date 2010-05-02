%if False

> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE
>     UndecidableInstances
>   , TypeOperators
>   , KindSignatures
>   , GADTs
>   , FlexibleContexts
>   , RankNTypes
>   #-}
> module Morphisms where

> import Control.Applicative
> import Control.Category
> import Control.Monad.Identity
> import Control.Monad.Reader hiding (mapM)
> import System.IO.Unsafe
> import Data.Foldable hiding (minimum)
> import Data.Traversable
> import Prelude hiding ((.), id, mapM, lookup)
> import Fixpoints

%endif

\begin{chapter}{Annotated Generic Traversals}
\label{chap:morphisms}
 
In the previous chapter we have seen how to associate functionality with
annotations. In this chapter will show how to write operations over annotated
recursive data structures. We make sure that all operations we write are
annotation unaware, which means we will use an existential annotation variable
in the type signature of our operations. This existential makes sure the
operations can be used in combination with all possible annotations.

Writing annotation-generic operations can only be done when the functions
cannot touch the recursive positions of the annotated datatypes, because the
recursive positions contain the annotations. We have to find a way to abstract
away from recursion when writing our algorithms.


In this chapter we will use a well known functional programming technique for
working with recursive datatypes. This technique has been explained by Meijer
et al. in their paper \emph{Functional programming with bananas, lenses,
envelopes and barbed wire.}\cite{bananas} We will introduce \emph{morphisms} to write
operations that abstract away from recursion. We will implement both an
annotation aware \emph{paramorphism} and \emph{apomorphism}. By writing
\emph{algebras} for these morphisms we will be able to destruct and construct
recursive data structures without explicitly touching the recursive positions.
By creating an endomorphic paramorphism and an endomorphic apomorphism we
will also be able to update existing recursive structures.

In the last part of this chapter we show how to combine multiple algebras into
one. This will allow us to perform multiple actions in a single tree traversal.
We will also investigate the effect of traversals for annotations that work
in a strict context on the running time of the operations.

\begin{section}{Paramorphisms}
\label{sec:para}

We start out by implementing a \emph{paramorphism}\cite{Paramorphisms}, a
bottom up traversal that can fold a recursive structure into a single value.  A
paramorphism is a generalization of the more commonly known
\emph{catamorphism}\cite{bananas}. The standard Haskell function |foldr|, which
can be used to destruct a list to a result value, is an example of a
catamorphism.

We first write down the type signature of the algebra for paramorphisms, we
call this algebra |Psi1|. 

> type Psi1 a f r = f (FixA a f :*: r) -> r

This type signature describes an algebra that should be able to produce an
value of type |r| from one single node containing both the fully annotated
sub-structures \emph{and} the recursive results of the paramorphic computation.

We now create a derived algebra type that hides the annotation variable inside
an existential quantification. This makes explicit that the algebras cannot
reason about the annotation.

> type Psi1A f r = forall a. Psi1 a f r

An example of such an algebra is the function |containsAlg| for binary
trees. This algebra describes a recursive traversal over a binary tree that
checks whether a certain integer value is included in the tree or not.

> containsAlg :: Int -> Psi1A Tree_f Bool
> containsAlg _  Leaf                      = False
> containsAlg v  (Branch c (_, l) (_, r))  = 
>   case v `compare` c of
>     LT  -> l
>     EQ  -> True
>     GT  -> r

Note that because the |containsAlg| algebra only uses the recursive
sub-results, and not the original sub-structures, this algebra is actually a
catamorphism, a special case of the more general paramorphism.  Because all
catamorphisms are paramorphisms this does not invalidate the example.

The paramorphism function performs a bottom up traversal over some
|Traversable| |Functor| and for every node applies the algebra, the result of
the algebra will be returned. The most generic version of this paramorphism
within our framework is the |paraMA1| function.  This function runs is some
monadic context |m| and performs a traversal over some annotated structure
|FixA a f| using the |AnnO| type class to perform annotation specific queries.

> paraMA1 :: AnnO a f m => Psi1 a f r -> FixA a f -> m r
> paraMA1 psi = return . psi <=< mapM (group (paraMA1 psi)) <=< annO
>   where group f c = fmap ((,) c) (f c)

From now on the $(_{\alpha}^m)$ postfix will be used to indicate that a
function requires a context and works on annotated structures.

The implementation of this generic paramorphism might seem a bit cryptic at
first sight, this is due to its very generic behaviour. Quickly summarized this
function performs a bottom-up traversal over a recursive structure like our
binary tree. As input it receives a fully annotated structure and it uses the
|annO| function to unwrap a single node out of the annotation.  The
|Traversable| instance, which is an implicit super class of the |AnnO| class,
allows us to use the |mapM| function to recursively apply the |paraMA1|
function to the sub-structures. This recursive invocation is used to come up
with the sub-results.  The sub-results will be grouped together with the
original sub-structures that these results are computed from. The original
input node with these grouped results as the values will be passed into the
algebra |psi|.  The algebra can now compute the result value for one level of
the recursive computation, possible using the results of deeper traversals.

To illustrate the usage of the |paraMA1| function we apply it to the
|containsAlg| algebra and get back a true function that performs a containment
check over a fully annotation binary tree.

> containsMA :: AnnO a Tree_f m => Int -> TreeA a -> m Bool
> containsMA v = paraMA1 (containsAlg v)

We can easily test this function in the interactive environment of the GHC
compiler.  We first manually construct a binary tree and constrain this to the
|IO| context and |Debug| annotation. While the binary tree is being
constructed, using our previously defined smart constructors, the debug
annotation prints out a trace of all nodes being produced.

\begin{small}
\begin{verbatim}
ghci> join (branchA 3 <$> leafA <*> leafA) :: IO (TreeA Debug)
annI: Leaf
annI: Leaf
annI: Branch 3 <D Leaf> <D Leaf>
<D Branch 3 <D Leaf> <D Leaf>>
\end{verbatim}
\end{small}

Now we can apply the |containsMA| function to the resulting binary tree and
check for the existence of a |Branch| with value |3|. While running this
function the debug annotation prints out a trace of all sub-structures being
queried.

\begin{small}
\begin{verbatim}
ghci> containsMA 3 it
annO: Branch 3 <D Leaf> <D Leaf>
annO: Leaf
annO: Leaf
True
\end{verbatim}
\end{small}

Note that the paramorphic traversal is as strict as the context it runs in.
This means that because the |Debug| annotation requires the |IO| monad the
|containsMA| function becomes more strict then necessary. In section
\ref{sec:laziness} we will describe a method to regain laziness for
paramorphisms running in strict contexts.

The paramorphism we have defined above is generic in the sense that it works
on structures with arbitrary annotations that run in an arbitrary context.
When an annotation does not have any requirements about the type of context to
run in, we can use the |Identity| monad to create a pure paramorphic traversal.

> paraA1 :: (AnnO a f Identity, Traversable f) => Psi1 a f r -> FixA a f -> r
> paraA1 psi = runIdentity . paraMA1 psi

When we further restrict the annotation to be the identity annotation, we get
back a pure paramorphism that works on plain unannotated structures.

> para1 :: Traversable f => Psi1 Id f r -> Fix f -> r
> para1 psi = paraA1 psi

To illustrate this pure paramorphism we apply it to the |containsAlg| algebra
and get back a pure |contains| function.

> contains :: Int -> Tree -> Bool
> contains v = para1 (containsAlg v)

In this section we have shown how to build an annotation aware paramorphism,
which can be applied to annotation-generic algebras. The |paraMA| function is
generic in both the annotation and the context the annotation requires. By only
restricting the types we can derive operations that operate over the pure,
in-memory variants of our data structures. In the next chapter will we do the
same for \emph{apomorphisms} which can be used to construct recursive data
structures from a seed value.

\end{section}

\begin{section}{Apomorphisms}
\label{sec:apomorphisms}

Dual to the paramorphism is the \emph{apomorphism}\cite{apomorphisms}. Where the paramorphism
abstract away from recursion, the apomorphisms abstracts away from corecursion.
Similarly, where paramorphisms use algebras to describe recursive operations,
apomorphisms use coalgebras to describe corecursive operations. Apomorphisms
are generalizations of the more well known \emph{anamorphisms}. The standard
Haskell function |unfold|, which can be used to create lists from a seed value,
is an example of an anamorphisms.

The coalgebra for an apomorphism, called |Phi|, takes a seed value of some type
|s| and must produce a node containing either a new seed or a
new recursive structure.

> type Phi a f s = s -> f (s :+: FixA a f)

From the type signature of the |Phi| coalgebra it is obvious that it is dual to
the |Psi| algebra for paramorphisms. Paramorphisms destruct recursive
structures to some result value |r|, apomorphisms construct recursive
structures from some seed value |s|.

We now create a derived coalgebra type that hides the annotation variable
inside an existential quantification. This makes explicit that the coalgebras
cannot reason about the annotation.

> type PhiA f s = forall a. Phi a f s

Because the annotation variable |a| is not accessible to coalgebras that are
written using the |PhiA| type synonym, it can never produce annotation values.
But the coalgebras is allowed to stop the corecursive traversal by using the
|Right| constructor to return a value of type |FixA a f|, an annotated
structure. There are three ways the coalgebras can use the second component of
the result sum type to stop the corecursion:

\begin{enumerate}
\item The first posibility for the coalgebra to stop the corecursive process is
to return an unannotated structure using the |Right| constructor. This can only
be done by using the |InF| fixed point constructor.
\item The coalgebra can \emph{reuse} an existing annotated structure it
receives as input. This can only work when the input is also an annotated
structure, and this can only be the case when the seed type is exactly |FixA a f|.
Apomorphisms that have a seed type equal to the result of the corecursion are
called \emph{endomorphic} apomorphisms, these will be discussed in section
\ref{sec:endoapo}.
\item The third options is to wrap an existing annotated structure
with one or more levels of unannotated nodes. This method is a combination of
method 1 and method 2.
\end{enumerate}

To illustrate the usage of coalgebras we define the function |fromListCoalg|
that describes how to create a balanced binary tree from an input list of
integer values.

> fromListCoalg :: PhiA Tree_f [Int]
> fromListCoalg []      = Leaf
> fromListCoalg (y:ys)  =
>   let  l  = take (length ys `div` 2) ys
>        r  = drop (length l) ys
>   in Branch y (Left l) (Left r)

Note that because the |fromListCoalg| coalgebra only produces new seeds using
the |Left| constructor, instead of directly producing sub-structures, it is
actually an anamorphism. Because all anamorphisms are apomorphisms this does
not invalidate the example.

Like the paramorphism we start with an apomorphism that corecursively generates
an annotated structure in some, possibly monadic, context. We call this
function |apoMA|. This apomorphism takes a coalgebra |Phi| and some initial
seed value |s| and uses this to produce an annotated structure |FixA a f|.

> apoMA :: AnnI a f m => Phi a f s -> s -> m (FixA a f)
> apoMA phi = annI <=< mapM (apoMA phi `either` fullyIn) . phi

This apomorphism first applies the algebra |phi| to the initial seed value |s|
to produce a new structure of type |f (s :+: FixA a f)|. This results has
either a new seed or a recursive sub-structure in the sub-positions. When we
encounter a new seed in the |Left| constructor we use the |apoMA| function to
recursively to produce a new sub-structure.  When we encounter a |Right|
constructor containg a (partially) annotated recursive structure again, we do
not go further into recusrion. Point 1 and 2 of the enumeration above show us
we are forced to reannotate the top of this structure. Because the coalgebra
can possibly create multiple levels of unannotated nodes, we have to use the
|fullyIn| function. When taken care of the sub-results the result will be
wrapped inside an annotation using the |annI| function.

Now we can apply the |apoMA| function to our example coalgebra |fromListCoalg|
and get back a function that can be used to produce annotated binary trees from
a list of integers.

> fromListMA :: AnnI a Tree_f m => [Int] -> m (TreeA a)
> fromListMA = apoMA fromListCoalg

To illustrate the usage of the |fromListMA| function we construct a simple
binary tree from a two-element lists. Again we constrain the context to |IO|
and the annotation to |Debug|. The annotation nicely prints out all the
sub-structures that are being produced before the final result tree is
returned

\begin{small}
\begin{verbatim}
ghci> fromListMA [1, 3] :: IO (FixA Debug (Tree_f Int))       
annI: Leaf
annI: Leaf
annI: Leaf
annI: Branch 3 <D Leaf> <D Leaf>
annI: Branch 1 <D Leaf> <D (Branch 3 <D Leaf> <D Leaf>)>
<D (Branch 1 <D Leaf> <D (Branch 3 <D Leaf> <D Leaf>)>)>
\end{verbatim}
\end{small}

Like we did for paramorphisms, we can specialize the |apoMA| function for
annotation types that do not require a context to run in. We use the identity
monad to get back a pure annotated apomorphism.

> apoA :: AnnI a f Identity => Phi a f s -> s -> FixA a f
> apoA phi = runIdentity . apoMA phi

Fixing the annotation to be the identity gives us back a pure apomorphism
working over structure without annotations.

> apo :: Traversable f => Phi Id f s -> s -> Fix f
> apo phi = apoA phi

Now we can simply create a pure |fromList| version working on plain binary
trees without annotations.

> fromList :: [Int] -> Tree
> fromList = apo fromListCoalg

In the last two section we have seen how to create recursive destructor and
constructor functions using an algebraic approach. The seed and result values
for both the paramorphisms and the apomorphisms were polymorph. The next two
sections show what happens when we move away from polymorphic result and seed
types to using annotated data structures as the result and seed types. This
will allow us to write modification functions on our annotated structures, like
\emph{insert} and \emph{delete}.

\end{section}

\begin{section}{Endomorphic paramorphism}
\label{sec:endopara}

Both the paramorphisms and the apomorphisms working on annotated structures had
enough information to know when to use the |annO| or |annI| functions to
wrap and unwrap annotations. The paramorphism starts out with querying the
value from the annotation before applying the algebra. The apomorphism produces
an annotation returned by the coalgebra. The algebras as defined in the
previous sections are very general in the sense that they can return a value of
any result type |r|. Some paramorphisms might choose to produce a value with a
type equal to the input type. 

We create two new type synonyms that describe \emph{endomorphic paramorphisms}.
The result value is fixed to the same type as the input type |FixA a f|. The
second type synonym additionaly hides the annotation variable |a| inside an
existential quantification.

> type Endo a  f = Psi1 a f (FixA a f)
>
> type EndoA   f = forall a. Endo a f 

The |Endo| type is an specialized version of the |Psi| type and describes an
algbera that returns either an existing fully annotated structure or produces a
new fully annotated structure.

> endoMA :: AnnIO a f m => Endo a f -> FixA a f -> m (FixA a f)
> endoMA psi = annIO (mapM fullyIn . outf . psi <=< mapM (group (endoMA psi)))
>   where group f c = fmap ((,) c) (f c)

The only real difference between the |paraMA| and the |endoMA| function is that
the latter knows it needs to use the |fullyIn| function on the result of the
algbera |psi|.  The |paraMA| function can be used to compute result values of
any types from an input structure, even unannotated forms of the input
structure.  The |endoMA| function can only be used to compute a fully annotated
structure with the same type as the input structure.

We illustrate the usage of the endomorphic paramorphisms using the
|replicateAlg| function. This algebra describes an operation that distributes a
single value to all the value positions in a the binary tree.

> replicateAlg :: Int -> EndoA Tree_f
> replicateAlg _    Leaf                     = InF Leaf
> replicateAlg v (  Branch _ (_, l) (_, r))  = InF (Branch v l r)

Because the |replicateAlg| produces a new structure it uses the |Right|
constructor from the sum-type. The |endoMA| morphism now knows it should
provide new annotations to the top of the result structure using the |fullyIn|
function.

Combinging the endomorphic paramorphism with the algbera for replication gives
us back a true replicate function for annotated structures.

> replicateMA :: AnnIO a Tree_f m => Int -> FixA a Tree_f -> m (FixA a Tree_f)
> replicateMA v = endoMA (replicateAlg v)

We can now test this function on the result of our prevouis example, the
expression |fromListMA [1, 3]|. The result shows a debug trace of how the
|replicateMA| function traverses the binary tree and builds up a new tree again
with the replicated value.

\begin{small}
\begin{verbatim}
ghci> replicateMA 4 it
annO: Branch 1 <D Leaf> <D (Branch 3 <D Leaf> <D Leaf>)>
annO: Leaf
annI: Leaf
annO: Branch 3 <D Leaf> <D Leaf>
annO: Leaf
annI: Leaf
annO: Leaf
annI: Leaf
annI: Branch 4 <D Leaf> <D Leaf>
annI: Branch 4 <D Leaf> <D (Branch 4 <D Leaf> <D Leaf>)>
<D (Branch 4 <D Leaf> <D (Branch 4 <D Leaf> <D Leaf>)>)>
\end{verbatim}
\end{small}

Like for regular paramorphisms we can create a specialized version that works for
annotation types that do not require a context to run in. We use the identity
monad to get back a pure annotated endomorphic paramorphism.

> endoA :: AnnIO a f Identity => Endo a f -> FixA a f -> FixA a f
> endoA psi = runIdentity . endoMA psi

Fixing the annotation to be the identity gives us back a pure endomorphic
paramorphism working over structure without annotations.

> endo :: Traversable f => Endo Id f -> Fix f -> Fix f
> endo psi = endoA psi

So, in this section we have seen how to specialize paramorphisms to work with
results values of the same type as the input values. Because the result values
when working with endomorphic paramorphisms is also an annotated structure, we
had to specialize the traversal function to produce annotations for the result
structure. In the next section we show how to specialize apomorphisms for have
annotated structures as the input seeds.

\end{section}

\begin{section}{Endomorphic apomorphisms}
\label{sec:endoapo}

Similar to the concept of endomorphic paramorphisms are the endomorphic
apomorphisms. Endomorphic apomorphisms are specific apomorphisms in the sense
that the input seed to produce new structures from is itself of the same
structure type. Endomorphic apomorphisms working on annotated structures suffer
from the same problem as their paramorphic counterparts: the apomorphic
traversal function |apoMA| it to generic to reason about annotated
seed values.

To allow writing proper endomorphic coalgebras for annotated structure we
introduce a two endomorphic coalgbera types. 

> type Coendo a f = Phi a f (FixA a f)
>
> type CoendoA  f = forall a. Coendo a f 

The |Coendo| type signature fixes the input seed to the type of the structure
that will be produced. The additional type |CoendoA| hides the annotation
variable inside an existential quantification.

We will now write the |coendoMA| function, that takes a endomorphic coalgbera
and an fully annotated input structure and produces an fully annotated output
structure. Note that both the endomorphic paramorphism and the endomorphic
apomorphism modify an input structure to an output structure of the same type.
Where the endomorphic paramorphisms takes in input algberas and the endomorphic
apomorphisms take coalgberas.

> coendoMA  ::  (Traversable f, AnnIO a f m)
>           =>  Coendo a f -> FixA a f -> m (FixA a f)
> coendoMA phi = annIO (mapM (coendoMA phi `either` fullyIn) . phi . InF)

The |coendoMA| morphism applies the coalgbera |phi| to the annotated input
structure throught the use of the |annIO| function from the |AnnIO| type class.
The |annIO| function makes sure the input structure is queried from the root
annotation and will be supplied a new annotation after applying the specified
function. After applying the coalgbera |phi| a case analysis will be done on
the result. Either a new seed is produced or an existing structure is reused,
similar to the regular apomorphisms. A new seed triggers a new recursive step,
an existing structure will be fully annotated.

As an example we will create an endomorphic coalgbera that describes the
insertion of one value into a binary tree. The seed value for this coalgbera is
an annotated binary tree of which the top level node is guaranteed not to have
an annotation\footnote{The fact that this node is always wrapped inside an
|InF| constructor and never in an |InA| constructor follows from the
implementation of the |coendoMA|, but is not encoded in the type.
Unfortunately, we cannot change the seed type to |f (FixA a f)| to encode this
invariant in the type, because this will also change the type of the seed we
have to produce.}. When the input is a |Leaf| we produce a singleton binary
tree with the specified input value. Because we use the |Right| constructor in
the recursive positions of the result, the traversals stops. When the input
value is a |Branch| we compare the value to be inserted with the value inside
this |Branch|. Based on the result we decide whether to insert the value in the
left or right sub-tree. When we want to insert the value into the right
sub-tree we stop the recursion with the |Right| constructor for the left
sub-tree and continue with a new seed using the |Left| constructor for the
right sub-tree.  When we want to insert the value into the left sub-tree we
perform the opposite task.

> insertCoalg :: Int -> CoendoA Tree_f
> insertCoalg v (InF s) =
>   case s of
>     Leaf          -> Branch v  (Right (InF Leaf))  (Right (InF Leaf))
>     Branch w l r  ->
>       case v `compare` w of
>         LT  -> Branch w  (Right l)           (Left  r)  
>         _   -> Branch w  (Left  l)           (Right r)

%if False

> insertCoalg _ (InA _) = error "something error happened"

%endif

Combining the endomorphic apomorphism with the endomorphic coalgbera for binary
tree insertion gives us back a true |insert| function on annotated binary
trees.

> insertMA :: AnnIO a Tree_f m => Int -> FixA a Tree_f -> m (TreeA a)
> insertMA v = coendoMA (insertCoalg v)

We can test the |insertMA| function by inserting the value |0| into the example
tree binary tree produced before with the |fromList [1, 2]|. The debug trace
shows the traversal that is being performed while inserting a new element into
the binary tree.

\begin{small}
\begin{verbatim}
ghci> insertMA 0 it
annO: Branch 1 <D Leaf> <D (Branch 3 <D Leaf> <D Leaf>)>
annO: Leaf
annI: Leaf
annI: Leaf
annI: Branch 0 <D Leaf> <D Leaf>
annI: Branch 1 <D (Branch 0 <D Leaf> <D Leaf>)>
               <D (Branch 3 <D Leaf> <D Leaf>)>
<D (Branch 1 <D (Branch 0 <D Leaf> <D Leaf>)>
             <D (Branch 3 <D Leaf> <D Leaf>)>)>
\end{verbatim}
\end{small}

And additionally, in the line of the other morphisms, we define two
specializations of this endomorphic apomorphism. The |coendoA| works for
annotations not requiring any context, the |coendo| working on pure structures
not requiring any context or annotation.

> coendoA :: AnnIO a f Identity => Coendo a f -> FixA a f -> FixA a f 
> coendoA phi = runIdentity . coendoMA phi

> coendo :: Traversable f => Coendo Id f -> Fix f -> Fix f
> coendo phi = coendoA phi

So, in this section we have seen how to specialize apomorphisms to work with
seed values of the same type as the output values.  Because the seed values are
now also annotated structures we had to specialize the traversal function to
query the structure out of the annotation before using it as the seed.

\end{section}

\begin{section}{Applicative paramorphisms}
\label{sec:paraapp}

In section \ref{sec:para} we have seen how to write some simple paramorphic
algebras. Writing more complicated algebras can be quite hard, mainly because
it is not always easy to compose multiple aspects of an operation into one
algebra. Combining multilpe algebras into a single algebra that can be used in
a single traversal is a well known problem in the world of functional
programming.  Attribute grammar systems like the \emph{UUAGC}\cite{ag} can be
used to combine different algebraic operations into a single traversal in an
aspect-oriented way. This section describes a more lightweight and idiomatic
approach to algebra composition. We make the paramorphic algebra type an
instance of Haskell's |Applicative| type class.

First we change the type for the algebra |Psi| from a type synonym into a real
datatype. We change the algebra into a real datatype, because this allows us to
add an additional constructor. As we will later see, this extra constructor is
needed for the |Applicative| instance.  The new |Psi| datatype will be a
generalized algebraic datatypes, or GADT, because we need an existentially
quantified type parameter later on.

> data Psi (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
>   Alg  :: (f (FixA a f, r) -> r)  -> Psi a f r

%if False

>   Prj  :: Psi a f (r -> s, r, s)  -> Psi a f s

> type PsiA f r = forall a. Psi a f r

> type EndoAppA a f = Psi a f (FixA a f)
> type EndoApp f = forall a. Psi a f (FixA a f)

%endif

The |Applicative| type class requires us to have a |Functor| instance and
requires us to implement two functions, |pure| and |<*>|.

\begin{spec}
class Functor f => Applicative f where
  pure   :: a -> f a
  (<*>)  :: f (a -> b) -> f a -> f b
\end{spec}

When we unify the type |f| with the the |Psi| type we get the following two
functions we have to implement in otder to have an |Applicative| instance. To
not get confused with our annotation variable we change the type variable |a|
and |b| to |r| and |s|.

\begin{spec}
pure   :: r -> Psi a f r
(<*>)  :: Psi a f (r -> s) -> Psi c f r -> Psi c f s
\end{spec}

From the signature for (|<*>|) it is clear that we should use an algebra that
produces a function from |a -> b| and an algebra that produces a |a| to build
an algebra that produces only a |b|. This applicative sequencing function
throws away any information about |a|. Because both the function and the
argument are needed \emph{in every stage of the traversal} we first create a
function |<++>| that simply groups together both the function |r -> s|, the
argument |r| and the result |s|.

> (<++>) :: Functor f => Psi a f (r -> s) -> Psi a f r -> Psi a f (r -> s, r, s)
> Alg f <++> Alg g = Alg $ \x -> mk  (f (fmap2 fst3 x))
>                                    (g (fmap2 snd3 x))
>   where  mk x y  = (x, y, x y)
>          fmap2   = fmap . fmap

This grouping function simply collect the results of the two input algebras,
the function and the argument, and results in a new algebra that combines these
two values and the application of the two.

%if False

> Prj  f <++> Prj  g = fmap trd3 f <++> fmap trd3 g 
> Alg  f <++> Prj  g = Prj (pure id <++> Alg f) <++> Prj g
> Prj  f <++> Alg  g = Prj f <++> Prj (pure id <++> Alg g)

%endif

%if False

> fst3 :: (a, b, c) -> a
> fst3 (x, _, _) = x

> snd3 :: (a, b, c) -> b
> snd3 (_, y, _) = y

> trd3 :: (a, b, c) -> c
> trd3 (_, _, z) = z

%endif

The next step in our attempt to create an |Applicative| instance for our
paramorphic algebras is to add an additional constructor to the |Psi| datatype.
This constructor projects the last component from the triple algebra created by
the grouping function. It uses the |Psi| GADT to create an existential that
hides the type variable |r| inside the |Prj| constructor.

\begin{spec}
Prj :: Psi a f (r -> s, r, s)  -> Psi a f s
\end{spec}

The new constructor requires us the to extend the grouping function with the
three additional cases involving the projection constructor. The implementation
is quite straightforward, but note that it uses the |pure| function from the
|Applicative| instance, which we have not defined yet.

\begin{spec}
(<++>) :: Functor f => Psi a f (r -> s) -> Psi a f r -> Psi a f (r -> s, r, s)
...
Prj  f <++> Prj  g = fmap trd3 f <++> fmap trd3 g 
Alg  f <++> Prj  g = Prj (pure id <++> Alg f) <++> Prj g
Prj  f <++> Alg  g = Prj f <++> Prj (pure id <++> Alg g)
\end{spec}

With the both the grouping function and the projection function we have enough
tools to create the |Applicative| instance for paramorphic algebras. The
applicative sequencing is a matter of grouping two input algebras together and
then only throwing the input types away using the existential from the GADT.
The pure function is an algebra that ignores the input structure and always
outputs the same value.

> instance Functor f => Applicative (Psi a f) where
>   pure     = Alg . const
>   a <*> b  = Prj (a <++> b)

We require a |Functor| super class instance for this |Applicative| instance
which can easily be made in terms of the |Applicative| type class.

> instance Functor f => Functor (Psi a f) where
>   fmap f h = pure f <*> h

The |Applicative| instance allows us to easily compose multiple algebras into
one, this grouped operation can now be applied to an input structure in one
traversal. This composability can help us to create more complicated algebras
without creating very big tuples representing all the components at once.

Because the algebra type |Psi| has become more complicated the |paraMA| should
be adapted to be able to produce both |Alg| and |Prj| constructors.

%if False

> instance Functor ((,,) a b) where
>   fmap f (a, b, c) = (a, b, f c)

> instance Foldable ((,,) a b) where
>   foldMap f (_, _, c) = f c

> instance Traversable ((,,) a b) where
>   traverse f (a, b, c) = (| ((,,) a b) (f c) |)

%endif

> paraMA :: AnnO a f m =>  Psi a f r -> FixA a f -> m r
> paraMA (Prj  psi) = fmap trd3 . paraMA psi
> paraMA (Alg  psi) = return . psi <=< mapM (g (paraMA (Alg psi))) <=< annO
>   where g f c = fmap ((,) c) (f c)

The implementation of this projection aware |paraMA| is not very different from
our original |paraMA|. The only difference is that this new version unpacks the
projection and applies the inner algebra. After applying only the result value,
the third component of the grouped triple, will be returned. The endomorphic
variant |endoMA| can be extended in the same trivial way, so we will not show
the implementation here.

%if False

> endoMApp :: AnnIO a f m => EndoAppA a f -> FixA a f -> m (FixA a f)
> endoMApp (Prj psi) = fmap trd3 . paraMA psi
> endoMApp (Alg psi) = fullyIn . psi <=< mapM (group (endoMApp (Alg psi))) <=< annO
>   where group f c = fmap ((,) c) (f c)

%endif

We can now illustrate the applicative paramorphisms using a well known example,
the |repmin| problem. The |repmin| problem describes a single traversal in
which every value in a structure, for example our binary tree, will be replaced
with the minimum value already in the structure. Our |Applicative| algebra
instance allows us to write the two aspects of this operation separately and
join them together using the sequencing operator |(<*>)|.

First we write down the algebra for computing the minimum value stored inside a
binary tree.

> minAlg :: PsiA Tree_f Int
> minAlg = Alg $ \a ->
>  case a of
>    Leaf          -> maxBound
>    Branch v l r  -> minimum [v, snd l, snd r]

And now we write the replication algebra that produces a \emph{function}. This
function produces the new binary tree given some input value. 

> repAlg :: PsiA Tree_f (Int -> TreeA a)
> repAlg = Alg $ \a x ->
>   case a of
>     Leaf          -> InF Leaf
>     Branch _ l r  -> InF (Branch x (snd l x) (snd r x))

Now we can write the |repmin| function by using the |endoMApp| function on the
applicative composition of the |repAlg| and the |minAlg|.

> repminMA :: AnnIO a Tree_f m => TreeA a -> m (TreeA a)
> repminMA = endoMApp (repAlg <*> minAlg)

In this chapter we have seen how to combine multiple algebras into one using the
Haskell |Applicative| type class. Writing more complicated operations as a
composition of separate algebras is generally more easy than writing monolithic
algebras containing multiple aspects in one.

\end{section}

\begin{section}{Lazy IO and strict paramorphisms}
\label{sec:laziness}

The paramorphism function working on annotated structures is as strict as the
context associated with the annotation. For example, the debug annotation works
in the |IO| monad, which makes all computations strict. This strictness can
have severe implications on the running time of the algorithms. In a strict
context all the sub results of the recursive computation will be computed, even
when the algebra discards them immediately. This is the reason that the debug
annotation in example \ref{sec:para} prints out far more sub-trees
than one would expect from a lazy traversal.

Some monads are lazy by default like the |Identity| monad and the |Reader|
monad. Using these lazy monads as the annotation context would make the
traversal naturally lazy. Some other monads are strict by default requiring all
computations on the left hand side of the monadic bind to be evaluated strictly
before the right hand side can be evaluated. Example of these monads are the
|IO| monad and the |State| monad. The first thing this section will show is how
we can make paramorphisms more lazy on the inside. Only sub-structures should
be traversed when the algebra requires them for the computation of the result
value. The goal is to make the running time of the paramorphic traversals in
strict contexts equivalent to the running time of pure traversals without any
context.

The decision about what sub-structures are needed for a certain computation is
up to the algebra and not to the paramorphism function itself. The algebra is a
pure description that is unaware of the annotation or associated context.
Because the paramorphism function does not know what information will be used by
the algebra it has to pass in all the recursive sub-results. To clarify this we
can look at the |containsAlg| for binary trees. As input this algebra get a
single structure with two booleans in the sub-structures. These booleans are
the indication whether the value is contained in one of the sub-structures.
Because we are dealing with a lazy language these sub-results are ideally not
computed yet, and will only be used when the algebras desires so. Running the
paramorphic traversal inside a strict context, like |IO| for our debug
annotation, will actually strictly precompute the recursive results before
passing them into the algebra. This changes the running time for the
|containsMA| function from the expected |O(log n)| to an unacceptable |O(n)|.

To solve the problem described above we introduce a type class |Lazy| that will
allow us to explicitly turn strict monads into lazy ones when this is possible.
The only class method is the function |lazy| that gets a monadic computation
and turns it into an lazy variant of this computation. Of course this will not
be possible in the most general case for all monads.

> class AM m => Lazy m where
>   lazy :: m a -> m a

Both the |Identity| monad and the |Reader| monad are lazy by default and can
trivially be made an instance of this type class. To be a bit more general we
make the |ReaderT| monad transformer an instance of the |Lazy| class for all
cases that it transforms another lazy monad.

> instance Lazy Identity where
>   lazy = id
>
> instance Lazy m => Lazy (ReaderT r m) where
>   lazy c = ask >>= lift . lazy . runReaderT c

Most interesting of all, we can also make |IO| an instance of the |Lazy| class
by using the |unsafeInterleaveIO| function. This function takes an |IO|
computation and produces an |IO| computation that will only be performed when
the result value is needed. This breaks the strict semantics of the |IO| monad,
which can become useful for our case.

> instance Lazy IO where
>   lazy = unsafeInterleaveIO

Now we have a way to enforce lazy semantics for some of the contexts our
traversals may run. By explicitly putting a call to |lazy| just before the
recursive invocations in the paramorphism we can make the entire traversal
lazy. The laziness of the computation is reflected in the function's class

> lazyParaMA :: (Lazy m, AnnO a f m) => Psi1 a f r -> FixA a f -> m r
> lazyParaMA psi = return . psi <=< mapM (group (lazy . lazyParaMA psi)) <=< annO
>   where group f c = fmap ((,) c) (f c)

When we now express the |containsMA| in terms of the more lazy paramorphism.

> containsMA2 :: (Lazy m, AnnO a Tree_f m) => Int -> FixA a Tree_f -> m Bool
> containsMA2 v = lazyParaMA (containsAlg v)

When we now run the |containsMA| functions on an example tree, in this case
created by |fromList [2, 1, 3]|, we see that the debug trace only prints out
the first element of the binary tree, because no other sub-results are needed
to come up with the answer.

\begin{small}
\begin{verbatim}
ghci> let tree = fromList [2, 1, 3]
ghci> containsMA 2 tree
annO: Branch 2  <D Branch 1 <D Leaf> <D Leaf>>
                <D Branch 3 <D Leaf> <D Leaf>>
True
\end{verbatim}
\end{small}

The GHCi debugger by default prints out the value the user requests at the
prompt, this is the only reason the expression |containsMA2 2 tree| is even
evaluated at all. The traversal has become become lazy on the inside, no
unneeded traversals will be performed, but also lazy on the outside, nothing
at all will happen until the answer is forced. This behaviour can be compared
to the |hGetContents| function from the Haskell prelude which uses lazy |IO|
\footnote{The |hGetContents| function internally also uses the
|unsafeInterleaveIO| function.} to give back the entire contents of a file or
socket resource as a single lazy |String|. This means the actual |IO| actions
to read the contents from file will only be performed when the individual
characters of the string are inspected. This behaviour can have a strange
outcome in practice, evaluation of pure code can have real world side effects.
The same is the case for our lazy traversals, the debug annotations only kicks
in when the result is inspected. While this behaviour is probably safe for the
debug annotation this might not be the case in general. There is a really good
reason the |unsafeInterleaveIO| function starts with the word `unsafe', there
are a lot of problems associated with lazy IO\cite{lazyioharmful}.

To make sure all the potential side effect stay within the context they belong to
we have to make sure our paramorphism remains strict on the outside. We do
this by forcing the entire result of the evaluation before returning the
answer. Using generic programming we have implemented a generic deep |seq|
function that can be used to force an entire computation fully and recursively
the moment it would normally only get forced \emph{weak head normal form}
(WHNF). The implementation internally uses the |seq| function from the Haskell
prelude that forces the first argument WHNF when the second argument gets
forced.

> dseq1  :: a -> a
> seq1   :: a -> b -> b

%if False

> dseq1 = undefined
> seq1 = undefined

%endif

We now apply the |dseq1| to the result of the paramorphism.

> paraMA' :: (Lazy m, AnnO a f m) => Psi1 a f r -> FixA a f -> m r
> paraMA' psi = dseq1 `liftM` lazyParaMA psi

By creating a new paramorphism function that forces the result before returning
we get a traversal that is lazy internally but is strict on the outside. As we
will see later when dealing with structures annotated with the persistent
annotations, this evaluation semantics is essential.

\end{section}

\end{chapter}

