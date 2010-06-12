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
> import Control.Monad.Identity
> import Control.Monad hiding (mapM)
> import Data.Traversable
> import Fixpoints hiding ((:+:))

%endif

\section{Recursion patterns}

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

The paramorphism function takes an algebra that compute a result value from a
one-level structure and uses this to recursively destruct an entire recursive
structure:

> paraA :: Out a f m => AlgA a f r -> FixA a f -> m r
> paraA (Psi p)  =    return . p
>                <=<  mapM (group (paraA (Psi p)))
>                <=<  outA . out
>   where group f c = liftM (,c) (f c)

\todo{literally explain this code?}
This paramorphism works on annotated structures; before computing the sub
results and before applying the algebra, it first unwraps the annotation for
the top-level node using the |outA| function. Using the |paraA| function we can
lift every paramorphic algebra to work on annotated structures without the
algbera having to know about the annotations. We make an additional algbera
type that hides the annotation variable inside an existential quantification:

> type Alg f r = forall a. AlgA a f r

When an algebra is built using the |Alg| type we know for sure it works for
every annotation type.

Now we build an example algebra that performs a value lookup by key in a binary
search tree:

> lookupAlg :: Ord k => k -> Alg (TreeF k v) (Maybe v)
> lookupAlg k = Psi $ \t ->
>   case t of
>     Leaf            -> Nothing
>     Branch c w l r  ->
>       case k `compare` c of
>         LT  ->  fst l
>         EQ  ->  Just w
>         GT  ->  fst r

We see the difference between this `lookup' algebra and the lookup function
from Section~\ref{sec:fixpoints}: the original function directly uses recursion
the find the value, the algebra reuses the subresults stored at the recursive
positions of the input node.

We can make the algebra into a real function again by applying the |paraA|
function to it:

> lookup  ::  (Ord k, Out a (TreeF k v) m)
>         =>  k -> TreeA a k v -> m (Maybe v)
> lookup k = paraA (lookupAlg k)

The algebra can be annotation agnostic, because it abstracts away from
recursion and outsources recursion to the paramorphic traversal function.

Another interesting example of a paramorphism is one that used a custom
function to maps all values inside a binary search tree into a monoid value and
combines the value into one using the monoid combinator |<>| (\texttt{mappend}
in Haskell).

> foldAlg :: Monoid m => (v -> m) -> Alg (TreeF k v) m
> foldAlg f = Psi $ \t -> case t of
>   Leaf            ->  mempty
>   Branch _ v l r  ->  fst l <> f v <> fst r

The |foldAlg| is a very generic algebra that can be specialized for a large
number of different Haskell types that have a |Monoid| instance. An example is
the |toList| function that uses the list monoid to deliver all values in a
binary search tree in a list:

> toList :: Out a (TreeF k v) m => TreeA a k v -> m [v]
> toList = paraA (foldAlg (\x -> [x]))

We test the |toList| function by applying it to the result of the list in
Section~\ref{sec:debug}. We see a full debug trace of all the unwrap steps
performed by paramorphic traversal:

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
paramorphisms. In Section~\ref{sec:modification} we see a morphism in which
also the original substructures are used in the algebra.

The algebras are written in pure style, no annotations appear in the algebra
and no monadic context is used. Using the annotated paramorphisms function we
interpreted the algebras in monadic context and apply the |outA| function to
the annotated structure where needed. We can also specialize the paramorphisms
function to only work for the identity annotation in the identity monad. Now we
gain pure operations on unannotated structures:

> para :: Traversable f => AlgA Id1 f r -> Fix f -> r
> para p = runIdentity . paraA p . runIdentity . fullyIn

By abstracting away from recursion we can reuse a single operation in different
contexts, both for annotated and unannotated recursive structures.

\subsection{Constructing with apomorphisms}
\label{sec:apomorphisms}

Where paramorphisms are used to destruct recursive datatypes into a result
value, \emph{apomorphisms} are used to construct recursive datatypes from a
seed value. Apomorphisms are generalizations of the more widely know
\emph{anamorphisms}. Apomorphisms build recursive structures from an initial
seed value and a \emph{coalebgra}. A coalebgra takes a seed value and produces
a single node with at the recursive positions either a new seed value or a or
an existing recursive structure:

> data CoalgA  (a ::  (  *  -> *) -> * -> *  )
>              (f ::     *  -> *             )
>              (s ::     *                   ) where
>   Phi :: (s -> f (s :+: FixBotA a f)) -> CoalgA a f s

Like with algebras for paramorphisms we define an coalgebra that hides the
annotation variable inside an existential quantification:

> type Coalg s f = forall a. CoalgA a f s

The apomorphisms functions |apoA| takes a coalgebra that produces a single
level datatype and itself corecursively produces an entire recursive datatype.
When the coalgebra produces a new seed value inside the node, the |apoA|
function recursively continues the construction.

> apoA :: In a f m => CoalgA a f s -> s -> m (FixA a f)
> apoA (Phi p)  =    return . In
>               <=<  inA
>               <=<  mapM (apoA (Phi p) `either` topIn) . p

As an example we now build a coalgebra that creates a binary search tree from a
list seed. We assume the seed is a sorted list. When the coalgebra receives an
empty list a |Leaf| is produces and the construction stops. In the case of a
non-empty list the middle element is stored in a branch, the left and right
remains of the list are used as new seeds for left and right subtrees.

> fromSortedListCoalg :: Coalg [(k, v)] (TreeF k v)
> fromSortedListCoalg = Phi $ \t ->
>   case t of
>     []  ->  Leaf
>     xs  ->  let  l        =  take ((length xs `div` 2) - 1) xs
>                  (k,v):r  =  drop (length l               ) xs
>             in Branch k v (Left l) (Left r)

We lift the coalgebra to a true function |fromList| using the annotated
apomorphism. Before applying the morphism the function sorts the input seed by
key to fulfil the invariant.

> fromList  ::  (In a (TreeF k v) m, Ord k)
>           =>  [(k, v)] -> m (FixA a (TreeF k v))
> fromList  =   apoA fromSortedListCoalg
>           .   sortBy (comparing fst)

We run the |fromList| in the |IO| context for the |Debug| annotation and see a
debug trace when the example tree gets created from the input seed:

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

Like with the paramorphism we can simply create a pure, unannotated variant of
the apomorphism by using the identity annotation in the identity monad:

> apo :: Traversable f => CoalgA Id1 f s -> s -> Fix f
> apo phi = runIdentity . fullyOut . runIdentity . apoA phi

\subsection{Modification with endomorphisms}
\label{sec:modification}

Paramorphisms are used to create query functions, apomorphisms are used to
create producer functions. There is still a large class of operations that
cannot be written using these two morphisms: the modification functions. In
this section we introduce an \emph{endomorphic apomorphisms} and use this to
write operations that takes as input a recursive structure and that produce as
output a recursive structure with the same type. We use this morphisms to build
modification functions like the |insert| function that inserts an new key/value
pair into an existing binary search tree.

First we define an additional annotation type class |OutIn| than combines the
two actions of unwrapping a node from- and wrapping a node in an annotation. A
function can be supplied to the |outInA| class method that is mapped to the
node inside the annotation, right after unwrapping and just before rewrapping
the node:

> class (Out a f m, In a f m) => OutIn a f m where
>   outInA  ::   (  f (FixA a f) -> m (   f (FixA a f)))
>           ->  a   f (FixA a f) -> m (a  f (FixA a f))
>   outInA f = inA <=< f <=< outA

As a super class it has both the |Out| and |In| classes. The super classes are
used to provide a default implementation that simply combines the wrapping and
unwrapping the annotation. The default implementation works is useful for some
annotation types, like the identity and debug annotation, but other annotation
types might profit from a custom implementation.

We build a helper function that lifts the |outInA| function to work in a node
inside the |In| constructor from the fixed point combinator:

> outIn1 :: OutIn a f m
>        => (f (  FixA a f)  -> m (f (  FixA a f)))
>        ->       FixA a f   -> m (     FixA a f)
> outIn1 f = return . In <=< outInA f . out

For the debug annotation we use the default implementation:

> instance  (Traversable f, Show (f ()))
>       =>  OutIn Debug f IO

In order to write modification function as algebras that abstract away from
recursion we define a additional morphism and accompanying coalgebra. We build
an endomorphic apomorphism, an apomorphism that takes as seed value a recursive
structure with the same type as the structure to produce.

The algebra type for the morphism takes as input seed a node with fully
annotated substructures and produces either a new seed with slightly modified
type |FixA a f| or a final recursive structure, possibly with an yet
unannotated top:

> data EndoA  (a  :: (  * -> *) -> * -> *  )
>             (f  ::    * -> *             ) where
>   PhiE :: (f (FixA a f) -> f (FixA a f :+: FixBotA a f)) -> EndoA a f

Because the input seed has type |f (Fix a f)| and the output seed has a
slightly different type |FixA a f| we cannot reuse the |CoalgA| type and use
this custom |EndoA| type.

Again, we make a derived type that hides the annotation variable:

> type Endo f = forall a. EndoA a f

The endomorphic apomorphism has a structure similar to that of the regular
apomorphism. The main difference is the use of the |outIn1| from the |OutIn|
type class to unwrap the incoming structure, apply the supplied coalgebra, and
wrap the result in an annotation:

> endoA  ::  OutIn a f m
>        =>  EndoA a f -> FixA a f -> m (FixA a f)
> endoA (PhiE phi) = outIn1 $
>   mapM (endoA (PhiE phi) `either` topIn) . phi

The endomorphic coalgebras that drive the modification operations have quite
some choice regarding their output:

\begin{itemize}
\item The coalgebras can produce and new seed to drive the next corecursive
step, this is indicated left part of the sum type in the |EndoA| datatype.
Coalgebras can use the helper function |next| to produce a new seed:

> next :: FixA a f -> Either (FixA a f) (FixBotA a f)
> next = Left

\item The coalgebra can reuse a fully annotated part of the input seed as the
final output. The helper function |stop| can be used to stop the modification
with an existing structure:

> stop :: FixA a f -> Either (FixA a f) (FixBotA a f)
> stop = Right . In . R . K

\item The last option for the coalgebra is to create one or more levels of new
nodes. The helper function |make| that takes a structure of type |FixBotA a f|
can be used to produce new node and stop the modification:

> make :: f (FixBotA a f) -> Either (FixA a f) (FixBotA a f)
> make = Right . In . L

\end{itemize}

Using the three helper function we create an example coalgebra for inserting
new key/value pairs into a binary search tree:

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
|insert| directly goes into recursion this function abstract away from
recursion using the three helpers functions.

We lift the |insertEndo| to a true insert function using the endomorphic
apomorphism:

> insert  :: (Ord k, OutIn a (TreeF k v) m)
>         => k -> v -> TreeA a k v -> m (TreeA a k v)
> insert k v = endoA (insertEndo k v)

In the this and the previous section we have shown a framework for
generically annotating recursive datatypes. Using an annotated fixed point
combinator we are able to store custom values at the recursive positions of
functional data structures, like the binary search tree. Using three different
type classes we are able to associate custom functionality to the construction,
destruction and modification of recursive structures. By writing algebras for
specific morphisms the operations abstract away from recursion, which allows us
to generically interleave the annotation instructions. We have shown three
different morphisms, one to produce query function, one for producer functions
and one for modification functions. The final functions we yield are applicable
to both annotated and unannotated structures.


