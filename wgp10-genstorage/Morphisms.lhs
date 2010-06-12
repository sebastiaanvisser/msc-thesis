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
> import Control.Monad hiding (mapM)
> import Data.Traversable
> import Fixpoints hiding ((:+:))

%endif

\section{Recursion patterns}

\todo[inline]{Idea from Sebas: Introduce only cata and ana, then endomorphic stuff}

In the previous section we have seen how to write recursive datatypes with an
additional type parameter for the recursive positions and using a fixed point
combinator to tie the knot. Using the fixed point combinator we are allowed to
store annotation variables at the recursive positions of the datatypes. The
|In| and |Out| type classes are used to associate custom functionality with the
construction and destruction of recursive datatype.

Writing operations on annotated datatypes seems comparatively hard:
\begin{itemize}
\item whenever we encounter a recursive position, we need to explicitly
  wrap or unwrap an annotation; \andres{I find this statement confusing. We have
the smart constructors for that, or not?}
\item because adding or removing an annotation is allowed to have a monadic
  effect, we have to write all operations in monadic style.
\end{itemize}
Fortunately, it turns out that we can abstract from all the tedious parts
in writing operations on annotated values by defining and subsequently
instantiating suitable recursion patterns.

We show three different types of operations and use a different pattern for
each type of operation.

\begin{itemize}
\item We use \emph{paramorphisms}~\cite{paras} to destruct recursive datatypes
to a single result value, these functions are called \emph{query} functions.
\item We use \emph{apomorphisms}~\cite{apos} to construct recursive datatypes
from a single seed value, these functions are called \emph{producer} functions.
\item We use \emph{endomorphic paramorphisms} to modify existing recursive
datatypes, these functions are called \emph{modifier} functions.\andres{Let's
think about the name. Perhaps \emph{modifier} is better than endomorphic
paramorphism.}
\end{itemize}

\todo[inline]{explain other morphisms might be useful, dependent on the input/ouput}

\andres[inline]{We perhaps have to put this section in context with existing
work, i.e., emphasize that none of the recursion patterns are really new, but
that we place them into our annotation framework here.}

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
> type a :+: b = Either a b

%endif

> data AlgA  (a  ::  (  *  -> *) -> * -> *  )
>            (f  ::     *  -> *             )
>            (r  ::     *                   ) where
>   Psi :: (f (r :*: FixA a f) -> r)  -> AlgA a f r

The function |paraA| then takes an algebra that computes a result value from a
one-level structure and uses the algebra to recursively destruct an entire
recursive structure:

> paraA :: Out a f m => AlgA a f r -> FixA a f -> m r
> paraA (Psi p)  =    return . p
>                <=<  mapM (group (paraA (Psi p)))
>                <=<  outA . out
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

> lookupAlg :: Ord k => k -> Alg (TreeF k v) (Maybe v)
> lookupAlg k = Psi $ \t ->
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

> lookup  ::  (Ord k, Out a (TreeF k v) m)
>         =>  k -> TreeA a k v -> m (Maybe v)
> lookup k = paraA (lookupAlg k)

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
\label{sec:apomorphisms}

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
>   Phi :: (s -> f (s :+: FixBotA a f)) -> CoalgA a f s

As for paramorphisms, we define a type synonym for coalgebras that do not
make use of a particular annotation:

> type Coalg s f = forall a. CoalgA a f s

The apomorphism function~|apoA| takes a coalgebra an applies it repeatedly
in order to produce an entire recursive structure. Wherever the coalgebra
produces a new seed value inside a node, the |apoA| function recursively
continues the construction.

> apoA :: In a f m => CoalgA a f s -> s -> m (FixA a f)
> apoA (Phi p)  =    return . In
>               <=<  inA
>               <=<  mapM (apoA (Phi p) `either` topIn) . p

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

> fromList  ::  (In a (TreeF k v) m, Ord k)
>           =>  [(k, v)] -> m (FixA a (TreeF k v))
> fromList  =   apoA fromSortedListCoalg
>           .   sortBy (comparing fst)

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
> apo phi = runIdentity . fullyOut . runIdentity . apoA phi

\subsection{Modification with endomorphisms}
\label{sec:modification}

Paramorphisms are used to create consumers, apomorphisms are used to
create producers. These functions hide the destruction and construction
of recursive values, and, in our case, the unwrapping and wrapping of
annotations from the user of these patterns. However, what about functions
that both consume and produce a recursive structure? Because we want to
abstract from all constructor and destructor applications, we cannot
express such modification functions using the recursion patterns discussed
so far. In this section, we introduce an
\emph{endomorphic apomorphism}\andres{Think about a better name}
and use this pattern to define modification operations such as the
insertion of a new key-value pair into an given binary search tree.

As a preparation, we define a type class~|OutIn| that combines the functionality
of the |Out| and |In| classes: using the class method |outInA|,
an annotated node is unwrapped, modified, and finally re-wrapped:

> class (Out a f m, In a f m) => OutIn a f m where
>   outInA  ::   (  f (FixA a f) -> m (   f (FixA a f)))
>           ->  a   f (FixA a f) -> m (a  f (FixA a f))
>   outInA f = inA <=< f <=< outA

We require both |Out| and |In| as superclasses of |OutIn|. Given the |outA|
and |inA| methods, we can supply a default implementation for |outInA|.
This default implementation is useful for some
annotation types, such as the identity and debug annotation.
Some other annotation types might profit from a custom implementation.\andres{Do
we have an example for this?}

The following helper function lifts the |outInA| function to work on a
fixed point:

> outIn1 :: OutIn a f m
>        => (f (  FixA a f)  -> m (f (  FixA a f)))
>        ->       FixA a f   -> m (     FixA a f)
> outIn1 f = return . In <=< outInA f . out

For the debug annotation, we use the default implementation:

> instance  (Traversable f, Show (f ()))
>       =>  OutIn Debug f IO

In order to write modification functions in an annotation-agnostic way, we
define an additional recursion pattern: the \emph{endomorphic apomorphism}
takes as seed value a recursive structure with the same type as the structure
to produce.

The algebra type for this pattern takes as input seed a node with fully
annotated substructures and produces either a new seed with slightly modified
type |FixA a f|, or a final recursive structure, possibly with a yet
unannotated top:

> data EndoA  (a  :: (  * -> *) -> * -> *  )
>             (f  ::    * -> *             ) where
>   PhiE :: (f (FixA a f) -> f (FixA a f :+: FixBotA a f)) -> EndoA a f

Because the input has type |f (Fix a f)| and the output has a
slightly different type |FixA a f|, we cannot just reuse the |CoalgA| type.%
\andres{perhaps we should remove this sentence}

Once more, we define a derived type that hides the annotation variable:

> type Endo f = forall a. EndoA a f

The modifier pattern has a structure similar to that of the regular
apomorphism. The main difference is the use of the |outIn1| from the |OutIn|
type class to unwrap the incoming structure, apply the supplied coalgebra, and
wrap the result in an annotation:

> endoA  ::  OutIn a f m
>        =>  EndoA a f -> FixA a f -> m (FixA a f)
> endoA (PhiE phi) = outIn1 $
>   mapM (endoA (PhiE phi) `either` topIn) . phi

The endo-algebras that drive the modification operations have significant
choice regarding their output:
\begin{itemize}
\item A new seed can be produced to drive the next recursive step, by
choosing the left part of the sum type in the result of |EndoA|. We
define the helper function |next| to give a more meaningful name to
the operation:

> next :: FixA a f -> Either (FixA a f) (FixBotA a f)
> next = Left

\item A fully annotated part of the input can be used as the
final output. We use the helper function |stop| for this purpose:

> stop :: FixA a f -> Either (FixA a f) (FixBotA a f)
> stop = Right . In . R . K

\item Finally, one or more levels of new nodes can be created, using
the helper function |make|, which takes a structure of type |FixBotA a f|
as its argument:

> make :: f (FixBotA a f) -> Either (FixA a f) (FixBotA a f)
> make = Right . In . L

\end{itemize}

Using the helper functions, we can now give an example of
a modifier: a function to insert a new key-value pair into a binary search
tree:

> insertEndo :: Ord k => k -> v -> EndoA a (TreeF k v)
> insertEndo k v = PhiE $ \s ->
>   case s of
>     Leaf -> Branch k v (make Leaf) (make Leaf)
>     Branch m w l r  ->
>       case k `compare` m of
>         LT  -> Branch m w (next  l) (stop  r)
>         _   -> Branch m w (stop  l) (next  r)

The structure of the |insertEndo| function is similar to that of the |insert|
function from Section~\ref{sec:fixpoints}. At the positions that the original
|insert| goes into recursion, we now call |next|. Trees are created using
either |make| or |stop|.

We run the |insertEndo| algebra by passing it to |endoA|:

> insert  ::  (Ord k, OutIn a (TreeF k v) m)
>         =>  k -> v -> TreeA a k v -> m (TreeA a k v)
> insert k v = endoA (insertEndo k v)

\subsection{Summary}

In this and the previous section we have shown a framework for
generically annotating recursive datatypes. Using an annotated fixed point
combinator we are able to store custom markers (containing potentially
custom information) at the recursive positions of
functional data structures. Using three different
type classes we are able to associate extra functionality with the construction,
destruction and modification of recursive structures. 

By defining algebras for specific recursion patterns, we can define functions
in a pure style, without having to worry about annotations or monadic contexts.
We have shown three frequently occurring
patterns, one for consumers, one for producers, and one for modifiers. More
patterns can be defined in a similar style if desired.
We can then use such operations in different contexts, as we have shown with
the debug and identity annotations. 

