%if False

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , FlexibleContexts
>   , DeriveFunctor
>   , DeriveFoldable
>   , DeriveTraversable
>   , StandaloneDeriving
>   , TypeOperators
>   , UndecidableInstances
>   , TemplateHaskell
>   , EmptyDataDecls
>   , TypeFamilies
>   #-}
> module Fixpoints where

> import Control.Monad hiding (mapM, (<=<))
> import Control.Monad.Identity (Identity(..))
> import Control.Monad.Trans
> import Data.Foldable hiding (sum)
> import Data.Monoid
> import Data.Traversable
> import Data.List
> import Data.Ord
> import Generics.Regular (deriveAll, PF)
> import Prelude hiding (mapM, sum)
> import Data.Time.LocalTime
> import Data.Time.Clock

%endif

\section{Working with fixed points}
\label{sec:fixpoints}

In this section, we repeat how datatypes can be rewritten as fixed points,
and algorithms working on such datatypes can be expressed in terms of
recursion patterns (Sections~\ref{sec:recdata} and \ref{sec:fix}).
Reexpressing datatypes and algorithms in this style
grants us fine-grained access to the structure of the datatypes, and
thereby control over the behaviour of operations. 

\subsection{Recursive datatypes}\label{sec:recdata}

As a running example of a typical recursive datatype,
we consider the datatype of binary search trees:

> data  Tree1 k v
>    =  Leaf1 | Branch1 k v (Tree1 k v) (Tree1 k v)

The type |Tree1| is parameterized over the type of keys~|k| and the
type of values~|v|.
The constructor |Branch1| represents an internal node, containing a key,
a value, a left and a right subtree. Leaves do not contain values and
are represented by |Leaf1|. We will maintain the binary search tree
property as an invariant. For simplicity, we will not try to keep the
tree properly balanced at all times.

\begin{figure}[tp]
\begin{center}
\includegraphics[scale=0.35]{img/binarytree.pdf}
\end{center}
\caption{An example of a binary tree.}
\label{fig:binarytree}
\end{figure}
An example tree, illustrated in Figure~\ref{fig:binarytree}, can be
defined as follows:

> myTree :: Tree1 Int Int
> myTree = Branch1 3 9  (Branch1 1 1   Leaf1 Leaf1) 
>                       (Branch1 4 16  (Branch1 7 49  Leaf1 Leaf1)
>                                      Leaf1)


We now present some simple operations on binary search trees. As many
functions that operate on datatypes, these examples follow the structure
of the datatype closely: they are instances of standard recursion patterns.

First, let us consider the |lookup| function on binary search trees. Given
a key, the function descends the tree. In each branch, the
argument is compared to the stored key in order to decide what branch to take. If
a correct key is found before a leaf is reached, the associated value is
returned.

> lookup1 :: Ord k => k -> Tree1 k v -> Maybe v
> lookup1 _  Leaf1              =  Nothing
> lookup1 k  (Branch1 n x l r)  =  case k `compare` n of
>                                    LT  ->  lookup1 k l
>                                    EQ  ->  Just x
>                                    GT  ->  lookup1 k r

Next, we define |fromList1|, a function that creates a binary search tree
from a list of key-value pairs. The function first sorts the list on the
keys and then calls a helper function |fromSortedList1|:

> fromList1 :: Ord k => [(k,v)] -> Tree1 k v
> fromList1 = fromSortedList1 . sortBy (comparing fst)
>
> fromSortedList1 :: [(k,v)] -> Tree1 k v
> fromSortedList1 []  =  Leaf1
> fromSortedList1 xs  =  
>   let (l, (k,v):r) = splitAt (length xs `div` 2 - 1) xs
>   in  Branch1 k v (fromSortedList1 l) (fromSortedList1 r)

If the input list is empty, a leaf is produced. Otherwise, we split the
list into two parts of approximately equal length, use the middle pair
for a new branch and call |fromSortedList1| recursively for both the left
and the right subtree.

Finally, we look at |insert1|, a function that inserts a new key-value pair into
a binary tree. Like |lookup1|, the function performs a key comparison to ensure
that the binary search tree property is preserved by the operation.

> insert1 :: Ord k => k -> v -> Tree1 k v -> Tree1 k v
> insert1 k v Leaf1              =  Branch1 k v Leaf1 Leaf1
> insert1 k v (Branch1 n x l r)  =  case k `compare` n of
>                                     LT  ->  Branch1 n x (insert1 k v l) r
>                                     _   ->  Branch1 n x l (insert1 k v r)

All three functions follow the structure of the |Tree1| datatype closely.
They recurse at exactly the places where the underlying datatype |Tree| is recursive.
Function |lookup1| destructs a tree, whereas |fromList1| builds one. The
function |insert1| modifies a tree, or destructs one tree while building
another.

\subsection{Fixed points}\label{sec:fix}

We now show how abstracting from the recursive positions in a datatype
and re-expressing the datatype as a fixed point of a functor
helps us to make the recursion patterns of the operations explicit.
For our running example, this means that we move
from |Tree1| to |TreeF| by adding a parameter~|r|
that is used wherever |Tree1| makes a recursive call:

> data TreeF k v r = Leaf | Branch k v r r

The type |TreeF| is also called the \emph{pattern functor} of |Tree|. 

To get our binary search trees back, we have to tie the recursive knot,
i.e., instantiate the parameter |r| with the recursive call. This job is
performed by the type-level fixed point combinator |Fix| that takes a
functor~|f| of kind~|* -> *| and parameterizes |f| with its own fixed
point:

> newtype Fix f = In { out :: f (Fix f) }

By using |Fix| on a pattern functor such as |TreeF|, we obtain a recursive
datatype once more that is isomorphic to the original |Tree1| datatype:

> type Tree k v = Fix (TreeF k v)

Building a binary tree structure for our new |Tree| type requires wrapping
all constructor applications with an additional application of the |In| constructor
of the |Fix| datatype. It is thus helpful to define ``smart constructors'' for
this task:

> leaf :: Tree k v
> leaf = In Leaf
>
> branch :: k -> v -> Tree k v -> Tree k v -> Tree k v
> branch k v l r = In (Branch k v l r)

Our example tree can now be expressed in terms of |leaf| and |branch|
rather than |Leaf1| and |Branch1|, but otherwise looks as before.
%if False

> myTreeF :: Tree Int Int
> myTreeF = branch 3 9  (branch 1 1   leaf
>                                     leaf) 
>                       (branch 4 16  (branch 7 49  leaf
>                                                   leaf)
>                                     leaf)

The structure of the tree is shown in Figure~\ref{fig:binarytreefix}.

\begin{figure}[tp]
\begin{center}
\includegraphics[scale=0.35]{img/binarytree-fix.pdf}
\end{center}
\caption{Binary tree with explicit recursion.}
\label{fig:binarytreefix}
\end{figure}
%endif

\subsection{Recursion patterns}\label{sec:simplerecpat}

Given a fixed point representation of a datatype, we can define a number
of recursion patterns for the datatype. But first, we have to back up the
fact that the pattern functor really is a functor. To this end, we make
it an instance of the |Functor| class:\footnote{Since version 6.12.1,
GHC can derive this instance automatically.}

> instance Functor (TreeF k v) where
>   fmap _ Leaf              = Leaf
>   fmap f (Branch k v l r)  = Branch k v (f l) (f r)

\paragraph{Catamorphism}

A \emph{catamorphism} is a recursion pattern that consumes a value
of a given data structure systematically. It is a generalization of
Haskell's |foldr| function to other datatypes:

> type Algebra f r = f r -> r
>
> cata :: Functor f => Algebra f r -> Fix f -> r
> cata phi = phi . fmap (cata phi) . out

The argument to the catamorphism is often called an \emph{algebra}. The
algebra describes how to map a functor where the recursive positions have
already been evaluated to a result. The function |cata| then repeatedly
applies the algebra in a bottom-up fashion to transform a whole recursive
structure.

The function |lookup| on binary search trees is an example of a catamorphism.
An algebra for |lookup| is defined as follows:

> lookupAlg :: Ord k => k -> Algebra (TreeF k v) (Maybe v)
> lookupAlg k Leaf              =  Nothing
> lookupAlg k (Branch n x l r)  =  case k `compare` n of
>                                    LT  ->  l
>                                    EQ  ->  Just x
>                                    GT  ->  r

Compared to the original definition of |lookup1|, the definition of |lookupAlg|
is not recursive. The benefit for us is that the new form facilitates changing the
behaviour of the function at recursive calls.

We can get the behaviour of the original |lookup1| function back by running
the algebra -- we simply pass it to the |cata| function:

> lookup :: Ord k => k -> Tree k v -> Maybe v
> lookup k = cata (lookupAlg k)

\paragraph{Anamorphism}

An \emph{anamorphism} is a recursive pattern that is dual to the catamorphism.
Where the catamorphism systematically decomposes a structure, the anamorphism
systematically builds one. It is a generalization of Haskell's |unfoldr| function
to other datatypes:

> type Coalgebra f s = s -> f s
>
> ana :: Functor f => Coalgebra f s -> s -> Fix f
> ana psi = In . fmap (ana psi) . psi

The argument to an anamorphism is called a \emph{coalgebra}. A coalgebra takes
a seed of type |s| and produces a functor |f s| where the elements contain new
seeds. The function |ana| repeatedly runs the coalgebra to all the seed values,
starting from the original seed, until none remain.

The function |fromSortedList1| is an anamorphism on trees. We can define
a suitable coalgebra as follows:

> fromSortedListAlg :: Coalgebra (TreeF k v) [(k,v)]
> fromSortedListAlg []  =  Leaf
> fromSortedListAlg xs  =
>   let (l, (k,v):r) = splitAt (length xs `div` 2 - 1) xs
>   in  Branch k v l r

The definition is very similar to the original one, but again, we have no
recursive calls, as those are handled by |ana| now:

> fromList :: Ord k => [(k,v)] -> Tree k v
> fromList = ana fromSortedListAlg . sortBy (comparing fst)

\paragraph{Apomorphism}

Let us recall the function |insert|. When we are in a branch of the tree,
we compare the stored key with the given key. Depending on the outcome, we
continue inserting into one subtree, but want to keep the other subtree
unchanged. While it is possible to coerce |insert| into both the catamorphism
and the anamorphism pattern, neither pattern is a very good fit.

Instead, we use an \emph{apomorphism}~\cite{apos} -- a generalization of an
anamorphism, and the dual concept of a paramorphism~\cite{paras}.

> type ApoCoalgebra f s = s -> f (Either s (Fix f))
>
> apo :: Functor f => ApoCoalgebra f s -> s -> Fix f
> apo psi = In . fmap apo' . psi
>   where  apo' (Left   l)  =  apo psi l
>          apo' (Right  r)  =  r

Where the coalgebra for anamorphisms generates a functor with new seeds
from a single seed, we can now decide at every recursive position whether
we want to produce a new seed (|Left|), or whether we simply want to provide a
recursive structure to use at this point (|Right|).

It is now easy to define a coalgebra for |insert|:

> insertAlg ::  Ord k => k -> v ->
>               ApoCoalgebra (TreeF k v) (Tree k v)
> insertAlg k v (In Leaf) =
>   Branch k v (Right (In Leaf)) (Right (In Leaf))
> insertAlg k v (In (Branch n x l r)) =
>   case compare k n of
>     LT  ->  Branch n x (Left   l) (Right  r)
>     _   ->  Branch n x (Right  l) (Left   r)
>
> insert :: Ord k => k -> v -> Tree k v -> Tree k v
> insert k v = apo (insertAlg k v)

We have now introduced three useful patterns that allow us
to define functions on a fixed point representation of a
datatype in such a way that we abstract from the recursive structure.
In the next section, we will make use of the abstraction by adding
new functionality in the form of annotations to the recursive
positions.

%if False

> (<>) :: Monoid a => a -> a -> a
> (<>) = mappend

\andres[inline]{I think that the flow here is suboptimal: We should first introduce
the fixed point combinator, the smart constructor, the catamorphism etc. The
whole paragraph on derived class instances is distracing at this point.
}

To gain more control over the recursive positions we create instances for the
|Functor|, |Foldable| and |Traversable| type classes. The instance
implementations are shown in figure \ref{fig:funcfoldtrav}.


\andres[inline]{At this point (or earlier), we have to introduce
catamorphisms and reiterate at least one of the example functions
as an explicit catamorphism.}

%endif

%if False

> $(deriveAll ''TreeF "PFTree") -- $
> type instance PF (TreeF k v f) = PFTree k v f

> deriving instance (Eq   k, Eq   v, Eq   f) => Eq   (TreeF k v f)
> deriving instance (Ord  k, Ord  v, Ord  f) => Ord  (TreeF k v f)
> deriving instance (Show k, Show v, Show f) => Show (TreeF k v f)

%endif

\section{Annotations}\label{sec:annotations}

By moving from a recursive datatype to its pattern functor, we now have
control over what exactly to do with recursive positions. We can simply
tie the knot using the fixed point combinator |Fix|, as we have seen above.
However, we can also store additional information at each recursive position.
In this section, we discuss how we can move from fixed poins to annotated
fixed points. We discuss how to create and remove annotations systematically,
and discuss example annotations.

\subsection{Annotated fixed points}

The \emph{annotated} fixed point combinator~|FixA| is defined as follows:

> type FixA ann f = Fix (ann f)

Instead of taking the fixed point of |f|, we take the fixed point of |ann f|,
where |ann| is a type constructor (of kind |(* -> *) -> * -> *|) that can be used to
modify the functor, for example by adding additional information.

The simplest annotation is the \emph{identity annotation}:

> newtype Id1 f a = Id1 { unId1 :: f a }

Using |FixA Id1| is isomorphic to using |Fix|.

We can define an annotated variant of binary search trees by applying
|FixA| in place of |Fix|:

> type TreeA ann k v = FixA ann (TreeF k v)

Once again, |TreeA Id1| is isomorphic to our old |Tree| type.

\subsection{Creating and removing annotations}

Our main goal in this article is to use annotations to represent pointers
to data that is stored on disk. Reading and writing to disk are effectful
operations. Therefore, we allow the creation and removal of annotations to
be associated with a monadic context.

We now define type classes |In| and |Out| that generalize the |In| and
|out| operations on fixed points to the annotated scenario. The method~|inA|
wraps a functor with fully annotated substructures and adds a new
annotation. The |outA| method unwraps an annotated node, exposing the
functor with the annotated substructures.
The results of both operations live in a monad~|m|:

> class Monad m => In ann f m where
>   inA :: f (FixA ann f) -> m (FixA ann f)
>
> class Monad m => Out ann f m where
>   outA :: FixA ann f -> m (f (FixA ann f)) 

The functor~|f| is added as a class parameter, so that we can impose
additional restrictions on it at a later stage.

For the identity annotation, we choose |m| to be the identity monad
(called |Identity|), and |inA| and |outA| are in essence just |In| and |out|:

> instance In Id1 f Identity where
>   inA = return . In . Id1
>
> instance Out Id1 f Identity where
>   outA = return . unId1 . out

Using the |In| type class, we define two new smart constructors for the annotated
binary search tree datatype:

> leafA :: In ann (TreeF k v) m => m (TreeA ann k v)
> leafA = inA Leaf
>
> branchA  ::  In ann (TreeF k v) m
>          =>  k -> v -> TreeA ann k v -> TreeA ann k v
>          ->  m (TreeA ann k v)
> branchA k v l r = inA (Branch k v l r)

The |leafA| and |branchA| smart constructors can be used to build up annotated
binary search trees for an arbitrary annotation type~|ann|. However, since the annotation type
is associated with a monadic context, we now have to build our example tree
in monadic style:

> myTree_a :: In ann (TreeF Int Int) m => m (TreeA ann Int Int)
> myTree_a =
>   do  l  <- leafA
>       d  <- branchA 7 49  l  l
>       e  <- branchA 1 1   l  l
>       f  <- branchA 4 16  d  l
>       branchA 3 9 e f

Note the type of |myTree_a|: the value is overloaded on the annotation~|ann|,
so we can use it with different annotations later.

\subsection{Example annotation: modification time}
\label{sec:modtime}

As a non-trivial example of an annotation, let us keep track of the
modification time of substructures. For this purpose, we define a
new datatype |ModTime1| that can be used as an annotation: next to
the actual structure, it also saves a |LocalTime|.\footnote{The |LocalTime| type is from
the Haskell \texttt{time} package.}

> data ModTime1 f a = M1 { time1 :: LocalTime, unM1 :: f a }

%if False
The following definition derives a non-record version of Show:

> data ModTime f a = M LocalTime (f a)
>   deriving Show

> time :: ModTime f a -> LocalTime
> time (M t _) = t

> unM :: ModTime f a -> f a
> unM (M _ x) = x

%endif

In order to use the annotation, we have to define instances of both the
|In| and |Out| classes, and thereby specify the behaviour associated with
creating and removing the annotation. In our case, we want to store the
current time when creating the annotation, but do nothing further
when dropping it:

> instance In ModTime f IO where
>   inA f = do  t <- getCurrentTime1
>               return (In (M t f))
>
> instance Out ModTime f IO where
>   outA = return . unM . out

%if False

> getCurrentTime1 :: IO LocalTime
> getCurrentTime1 =
>   do zone <- getCurrentTimeZone
>      utcToLocalTime zone `liftM` getCurrentTime

%endif
Because getting the current time requires a side effect, the |ModTime|
annotation is associated with the |IO| monad. 

We can now use the annotation, for example with our binary search tree
type, which we specialize to use the |ModTime| annotation:

> type TreeM k v = TreeA ModTime k v

As a simple use case, we can evaluate the overloaded example tree |myTree_a|
using the modification time annotation simply by specializing its type
accordingly. The construction of the tree has to take place in the |IO|
monad:
\begin{verbatim}
ghci> myTree_a :: IO (TreeM Int Int)
{M 236807 (Branch 3 9
  {M 236755 (Branch 1 1
    {M 236688 Leaf}
    {M 236688 Leaf})}
  {M 236781 (Branch 4 16
    {M 236728 (Branch 7 49
      {M 236688 Leaf}
      {M 236688 Leaf})}
    {M 236688 Leaf})})}
\end{verbatim}
For readability, we have cropped the modification times to the microseconds,
reformatted the output slightly to resemble the tree structure, and used a custom
|Show| instance for |Fix| that uses curly braces for the |In|
constructor. The tree is also shown schematically in Figure~\ref{fig:binarytreeann}.

The last modification times of all the leaves are the same, because
we share the result of one call to |leafA| in the definition of |myTree_a|. We
see that the modification times follow the order of the monadic operations in
|myTree_a|: the leaves are created first, the root of the tree is created last.

\begin{figure}[tp]
\begin{center}
\includegraphics[scale=0.35]{img/binarytree-M.pdf}
\end{center}
\caption{Binary tree with local modification times saved as annotations at the
recursive positions.}
\label{fig:binarytreeann}
\end{figure}

\subsection{Example annotation: debug trace}
\label{sec:debug}

As another example of an annotation we introduce |Debug1|:

> newtype Debug1 f a = D1 { unD1 :: f a }

This annotation does not store any data except for the functor
itself. We are only interested in the effects associated with
creation and removal of the annotation: we want to create a debug
trace of these operations when they occur.

%if False
The following definition derives a non-record version of Show:

> newtype Debug f a = D (f a)
>   deriving Show

> unD :: Debug f a -> f a
> unD (D x) = x

%endif

The desired behaviour is implemented in the |In| and |Out| instances
for the debug annotation:

> instance (Functor f, Show (f ())) => In Debug f IO where
>   inA = return . In . D <=< printer "in"
>
> instance (Functor f, Show (f ())) => Out Debug f IO where
>   outA = printer "out" . unD . out

Here,

> (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
> (f <=< g) x = g x >>= f

%if False

> infixr 1 <=<

%endif
is right-to-left Kleisli composition.\footnote{Available as |<<=<<| in Haskell.}
It has lower precedence than normal function composition.
The function |printer| prints the top layer of a given recursive structure~|f|
and also returns~|f|:

> printer :: (Functor f, Show (f ())) => String -> f a -> IO (f a)
> printer s f = print (s, fmap (const ()) f) >> return f


To use the debug annotation, we first specialize our binary
tree type:

> type TreeD k v = TreeA Debug k v

Then, we evaluate |myTree_a| once more, this time using |TreeD|
as a result type. This causes a trace of all the contruction steps
to be printed:
%if False

> instance Show (f (Fix f)) => Show (Fix f) where
>   show (In f) = "{" ++ show f ++ "}"

%endif
\begin{verbatim}
ghci> myTree_a :: IO (TreeD Int Int)
("in",Leaf)
("in",Branch 7 49 () ())
("in",Branch 1 1 () ())
("in",Branch 4 16 () ())
("in",Branch 3 9 () ())
{D (Branch 3 9 {D (Branch 1 1 {D Leaf}
{D Leaf})} {D (Branch 4 16 {D (Branch 7
49 {D Leaf} {D Leaf})} {D Leaf})})}
\end{verbatim}
%if False

No need to show two annotated trees in figures ...

\begin{figure}[tp]
\begin{center}
\includegraphics[scale=0.35]{img/binarytree-ann.pdf}
\end{center}
\caption{Binary tree with annotations.}
\label{fig:binarytreeann}
\end{figure}

%endif

\subsection{Summary}

In this section, we have introduced annotated fixed points. We have discussed
how to abstract from the creation and removal of annotations by means of the
|In| and |Out| type classes. We have also introduced two example annotations,
both with effects in the |IO| monad: one to keep track of the modification time
of substructures, and one to generate a debug trace of operations on the
structure. In Section~\ref{sec:storage}, we will introduce an annotation that
allows us to make data structures persistent.

Before that, we have a closer look at how to work with annotated structures.
Until now, we have seen that values defined in a monadic style such as |myTree_a|
can be evaluated at different annotation types, leading to different behaviour.
In the next section, we show how the recursion patterns we introduced in
Section~\ref{sec:simplerecpat} can be lifted to the annotated scenario, meaning
that we can also lift functions specified via algebras to work on annotated structures
easily.

%if False

\subsection{Multi level annotations}

\andres[inline]{I do not like this subsection. It has no motivation, and
it looks like |fullyIn| and |fullyOut| should be instances of proper
recursion patterns and not be defined directly.}

Both the |inA| and |outA| functions work on a single level of an annotated
recursive datatype. In terms of these two functions we define two functions that
respectively annotate or unannotate an entire recursive structure:

> fullyIn :: (Traversable f, In a f m) => Fix f -> m (FixA a f)
> fullyIn = inA <=< mapM fullyIn <=< return . out
>
> fullyOut :: (Traversable f, Out a f m) => FixA a f -> m (Fix f)
> fullyOut = return . In <=< mapM fullyOut <=< outA

The |fullyIn| function takes an unannotated recursive datatype |Fix f| and
wraps all the nodes recursively in a fresh annotation producing a fully
annotated recursive datatype |FixA a f|. The |fullyOut| performs dual task of
taking an annotated structure |FixA a f| to an unannotated structure |Fix f|.

When we use the |fullyOut| function on the result of our previous
command\footnote{In the interactive environment of the Glasgow Haskell compiler
you can use the \texttt{it} keyword to refer back to the result of the previous
command.} we see a full trace of the unwrapping of all the |D| constructors:

\begin{verbatim}
ghci> fullyOut it
("out",Branch 3 9 () ())
("out",Branch 1 1 () ())
("out",Leaf)
("out",Leaf)
("out",Branch 4 16 () ())
("out",Branch 7 49 () ())
("out",Leaf)
("out",Leaf)
("out",Leaf)
{Branch 3 9 {Branch 1 1 {Leaf} {Leaf}} {Branch
4 16 {Branch 7 49 {Leaf} {Leaf}} {Leaf}}}
\end{verbatim}

%endif
