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

> import Data.Foldable hiding (sum)
> import Data.Traversable
> import Prelude hiding (mapM, sum)
> import Control.Monad hiding (mapM)
> import Control.Monad.Identity hiding (mapM)
> import Generics.Regular (deriveAll, PF)

%endif

\section{Working with fixed points}
\label{sec:fixpoints}

\andres[inline]{Introduction: First fixed points. Well known. Lots of
related work. Then annotated fixed points and example annotations.}

\subsection{Recursive datatypes}

Interesting datatypes are usually recursive. Here is an example -- a datatype
for binary search trees storing both keys and values:
\andres{Since we insist that this is a BST, we have to say something about
the BST property somewhere.}

> data  Tree1 k v
>    =  Leaf1 | Branch1 k v (Tree1 k v) (Tree1 k v)

The constructor |Branch1| represents an internal node, containing a key,
a value, a left and a right subtree. Leaves do not contain values and
are represented by |Leaf1|.

Using the constructors |Leaf1| and |Branch1|, we can build an example
binary search tree:

> myTree :: Tree1 Int Int
> myTree = Branch1 3 9  (Branch1 1 1   Leaf1
>                                      Leaf1) 
>                       (Branch1 4 16  (Branch1 7 49  Leaf1
>                                                     Leaf1)
>                                      Leaf1)

This example binary tree is illustrated in Figure~\ref{fig:binarytree}.

\begin{figure}[tp]
\label{fig:binarytree}
\begin{center}
\includegraphics[scale=0.35]{img/binarytree.pdf}
\end{center}
\caption{An example of a binary tree.}
\end{figure}

Functions that operate on a datatype often follow the structure of the
datatype closely. If the underlying datatype is recursive, the function
is recursive as well. Consider as an example the |lookup| function on
binary search trees: it takes a key and a tree and recursively descends
the tree. In each branch, the stored key is compared with the given key.
Depending on the outcome of the comparison, the left or right subtrees are
traversed, or the value is returned. If the traversal reaches a leaf,
nothing is returned.

> lookup1 :: Ord k => k -> Tree1 k v -> Maybe v
> lookup1 _  Leaf1              =  Nothing
> lookup1 k  (Branch1 n x l r)  =  case k `compare` n of
>                                    LT  ->  lookup1 k l
>                                    EQ  ->  Just x
>                                    GT  ->  lookup1 k r

Another example is the |insert| function that inserts a new key-value pair into
a binary tree. Like |lookup1|, the function performs a key comparison to ensure
that the binary search tree property is preserved by the operation.
\andres{We might want to mention the absence of rotations in a footnote.}

> insert1 :: Ord k => k -> v -> Tree1 k v -> Tree1 k v
> insert1 k v Leaf1              =  Branch1 k v Leaf1 Leaf1
> insert1 k v (Branch1 n x l r)  =  case k `compare` n of
>                                     LT  ->  Branch1 n x (insert1 k v l) r
>                                     _   ->  Branch1 n x l (insert1 k v r)

Both |lookup1| and |insert1| follow a similar pattern. They recurse
at exactly the places where the underlying datatype |Tree| is recursive.
In the following, we are going to make the use of recursion in the datatype
explicit, and abstract from the common pattern.

\subsection{Fixed point combinator}

The first step in making the use of recursion in a datatype explicit is
to abstract from it. We move from |Tree1| to |TreeF| by adding a parameter~|r|
that is used whereever |Tree1| makes a recursive call:

> data TreeF k v rr = Leaf | Branch k v rr rr
>   deriving (Functor, Foldable, Traversable)

\andres{|Foldable| is required as a superclass of |Traversable|, but confusing.}
The type |TreeF| is also called the \emph{pattern functor} of |Tree|. In the
rest of this paper, we refer to datatypes defined via their pattern functor as
\emph{open} recursive datatypes.

\andres[inline]{I think that the flow here is suboptimal: We should first introduce
the fixed point combinator, the smart constructor, the catamorphism etc. The
whole paragraph on derived class instances is distracing at this point. Furthermore,
I think it makes sense to actually give the instances, and perhaps say they can also
be derived. That makes it trivial to move them to a later point.}

To gain more control over the recursive positions we automatically
derive\footnote{Using the Glasgow Haskell Compiler |>=| 6.12.} the |Functor| and
|Traversable| type classes. The |fmap| method from the |Functor| class can be
used to generically map a function over the values of container datatype.  The
|mapM| method from the |Traversable| class is similar to |fmap| but can work in
a monadic context:

\begin{spec}
fmap  :: Functor f      => (a ->    b) -> f a ->      f b

mapM  :: Traversable f  =>
         Monad m        => (a -> m  b) -> f a -> m (  f b)
\end{spec}

Note that the |Functor| and |Traversable| instances for the |TreeF| type work
on the additional type parameter for the recursive positions and not on the key
or value types.

We now introduce the type level fixed point combinator |Fix| that takes a type
constructor |f| of kind |* -> *| and parametrizes |f| with its own fixed point.

> newtype Fix f = In { out :: f (Fix f) }

By using |Fix| on a pattern functor such as |TreeF|, we obtain a recursive
datatype once more that is isomorphic to the original |Tree1| datatype:

> type Tree k v = Fix (TreeF k v)

Building a binary tree structure for our new |Tree| type requires wrapping
all constructor applications with an additional application of the |In| constructor
of the |Fix| datatype. It is helpful to define \emph{smart constructors} for
this task:

> leaf :: Tree k v
> leaf = In Leaf
>
> branch :: k -> v -> Tree k v -> Tree k v -> Tree k v
> branch k v l r = In (Branch k v l r)

The example tree now becomes

> myTreeF :: Tree Int Int
> myTreeF = branch 3 9  (branch 1 1   leaf
>                                     leaf) 
>                       (branch 4 16  (branch 7 49  leaf
>                                                   leaf)
>                                     leaf)

and is shown in Figure~\ref{fig:binarytreefix}.\andres{The
figure is using $\mu$ rather than |In|.}

\begin{figure}[tp]
\label{fig:binarytreefix}
\begin{center}
\includegraphics[scale=0.35]{img/binarytree-fix.pdf}
\end{center}
\caption{Binary tree with explicit recursion.}
\end{figure}

\andres[inline]{At this point (or earlier), we have to introduce
catamorphisms and reiterate at least one of the example functions
as an explicit catamorphism.}

%if False

> $(deriveAll ''TreeF "PFTree") -- $
> type instance PF (TreeF k v f) = PFTree k v f

> deriving instance (Eq   k, Eq   v, Eq   f) => Eq   (TreeF k v f)
> deriving instance (Ord  k, Ord  v, Ord  f) => Ord  (TreeF k v f)
> deriving instance (Show k, Show v, Show f) => Show (TreeF k v f)

%endif

\subsection{Fixed point annotations}\label{sec:annotations}

By moving from a recursive datatype to an open recursive datatype, we now
have control about what to do with recursive positions. As we have just seen,
we can plug everything together as before using the fixed point combinator |Fix|.
However, we can now decide to also do something else. In particular, we can
decide to store additional information at each recursive position.

For this purpose, we introduce the \emph{annotated} fixed point combinator~|FixA|:

> type FixA ann f = Fix (ann f)

Instead of taking the fixed point of |f|, we take the fixed point of |ann f|,
where |ann| is a type constructor (of kind |(* -> *) -> * -> *|) that can be used to add
additional information to the datatype.

We now make an annotated binary search tree by applying the |FixA| combinator
to our tree functor:

> type TreeA ann k v = FixA ann (TreeF k v)

If we instantiate |ann| with the type-level identity

> newtype Id1 f ix = Id1 { unId1 :: f ix }

we once again obtain a type that is isomorphic to our original |Tree1|. 
We return to the identity annotation in Section~\ref{sec:identity}.
In Section~\ref{sec:debug}, we present an example of a non-trivial annotation.

Building an annotated binary search tree of type |TreeA| requires wrapping the
non-recursive nodes in both an |In| constructor from the fixed point combinator
and adding an annotation. We introduce a type class |In| that enabled us to wrap
a single node in an annotation. The |inA|
method takes a single node with \emph{fully annotated sub-structures} and wraps
the node in an annotation type~|ann|. As we will see, annotating values can be
associated with effects. Therefore, we place the result of |inA| in a
monadic context~|m|:\andres{I think it would be good to give the instance for
the identity annotation immediately. Is it clear why |Traversable| should
be a superclass?}

> class (Traversable f, Monad m) => In ann f m where
>   inA :: f (FixA ann f) -> m (ann f (FixA ann f))

Using the |In| type class, we define two new smart constructors for the annotated
binary tree datatype:

> leafA :: In ann (TreeF k v) m => m (TreeA ann k v)
> leafA = In `liftM` inA Leaf
>
> branchA  ::  In ann (TreeF k v) m
>          =>  k -> v -> TreeA ann k v -> TreeA ann k v
>          ->  m (TreeA ann k v)
> branchA k v l r = In `liftM` inA (Branch k v l r)

The |leafA| and |branchA| smart constructors can be used to build up annotated
binary search tree for some annotation type |ann|. Because the annotation type
is associated with a monadic context we now build our example tree in monadic
style:

> myTree_a :: In ann (TreeF Int Int) m => m (TreeA ann Int Int)
> myTree_a =
>   do  l  <- leafA
>       d  <- branchA 7 49  l  l
>       e  <- branchA 1 1   l  l
>       f  <- branchA 4 16  d  l
>       branchA 3 9 e f

Note the type of |myTree|: The value is overloaded on the annotation, so we
can use it with different annotations later.

\andres{Is another figure needed here?}

The dual of the |In| type class is the |Out| type class that is used to unwrap
values from an annotation type. The |outA| method takes an annotated structure
with fully annotated substructures and unwraps the annotation to come up with a
node |f| with fully annotated structures at the recursive positions. Again,
this class method works in some monadic context~|m|.

> class (Traversable f, Monad m) => Out ann f m where
>   outA :: ann f (FixA ann f) -> m (f (FixA ann f)) 

\andres[inline]{Again, I think we should show an instance, for example for the
identity annotation. Also, we are getting ahead of things at this point, because
we introduce abstraction without seeing the need for it.}

\subsection{Example annotation: debug trace}
\label{sec:debug}

As an example of an annotation type we introduce the |Debug| annotation.
Using |Debug|, we can produce a debug trace of all construction and destruction
steps that an operation performs.

> newtype Debug1 f a = D1 { unD1 :: f a }

%if False
The following definition derives a non-record version of Show:

> newtype Debug f a = D (f a)
>   deriving Show

> unD :: Debug f a -> f a
> unD (D x) = x

%endif
The |Debug| type does not actually store any additional information. It
just serves as a marker that we can subsequently use to add additional
behaviour.\andres{This is actually a bit sad. It does not serve well to
motivate why we need the full power of annotations. At the very least,
add a forward pointer to a more interesting use.}

The behaviour is added when constructing or deconstructing values. As
we have seen in Section~\ref{sec:annotations}, constructors add annotations
by calling |inA|, whereas values are extracted from annotations using |outA|.
The |In| instance for the |Debug| type prints a message that indicates which
value is being constructed, and then returns that value:\andres{verify}

> instance  (Traversable f, Show (f ())) => In Debug f IO
>    where  inA = return . D <=< printer "in"

\andres[inline]{The following paragraph on Kleisli composition breaks the
flow. I would either cut it down sufficiently to be able to ban it to a
footnote, or get rid of Kleisli composition completely. Instead, |printer|
has to be explained immediately.}

Throughout this paper the |<=<| operator (|<<=<<| in Haskell) is used for right-to-left
Kleisli composition. When both used in the same expression, normal
function compositions takes precedence over Kleisli composition. The type
of the |<=<| operator is:

< Monad m => (b -> m c) -> (a -> m b) -> a -> m c

The |Out| instance for |Debug| drops the annotation marker, prints
the extracted value and returns it:

> instance  (Traversable f, Show (f ())) => Out Debug f IO
>    where  outA = printer "out" . unD

Both class methods use a helper function |printer| that print a single level of
a recursive structure to the console.\andres{Too late, see above.} 

> printer :: (Functor f, Show (f ())) => String -> f a -> IO (f a)
> printer s f = print (s, fmap (const ()) f) >> return f

We specialize our annotated binary tree to a debug tree as follows:

> type TreeD k v = TreeA Debug k v


As a simple example, let us specialize the type
of our sample tree |myTree| to make use of the debug annotation. The
construction then has to take place in the |IO| monad, and will print a
trace of all the construction steps involved:

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
For the ease of reading we use a special |Show| instance for the |Fix| type
that prints the inner structure within curly braces instead of printing an
explicit |In| constructor.

\begin{figure}[tp]
\label{fig:binarytreeann}
\begin{center}
\includegraphics[scale=0.35]{img/binarytree-ann.pdf}
\end{center}
\caption{Binary tree with annotations.}
\end{figure}

\subsection{Multi level annotations}

Both the |inA| and |outA| functions work on a single level of an annotated
recursive datatype. In terms of these two function we define two functions that
respectively annotate or unannotate an entire recursive structure:

> fullyIn :: In a f m => Fix f -> m (FixA a f)
> fullyIn = return . In <=< inA <=< mapM fullyIn . out
>
> fullyOut :: Out a f m => FixA a f -> m (Fix f)
> fullyOut = return . In <=< mapM fullyOut <=< outA . out

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

\subsection{Partially annotated structures}
\todo{maybe introduce this when needed}

We define an additional type that represents a recursive structure of which the
top contains no annotations but the sub structures are fully annotated. The
unannotated top may contains multiple levels. The |FixBotA| type uses the fixed
point combinator with in combination with a sum type:

> type FixBotA a f = Fix (f :+: K (FixA a f))

where:

> newtype K f a = K { unK :: f }
> data (f :+: g) a = L (f a) | R (g a)

%if False

> infixl 6 :+:

%endif

The left part of the sum represents an unannotated top node, the right part of
the sum contains a fully annotated bottom structure. The constant functor |K|
is used to ignore the incoming type index that the fixed point combinator
supplies, which stops the recursion and ensures there are no unannotated nodes
in a sub-structure of an annotated node.

A helper function |topIn| can be used to wrap the unannotated top part of a
|FixBotA| structure in fresh annotations and build a fully annotated structure:

> topIn :: In a f m => FixBotA a f -> m (FixA a f)
> topIn = sum  (return . In <=< inA <=< mapM topIn)
>              (return . unK) . out
>   where  sum f _  (L  l)  = f  l
>          sum _ g  (R  r)  = g  r

\subsection{Identity annotations}\label{sec:identity}

With the debug annotation we have shown how to associate custom functionality
with the construction and destruction of recursive datatypes. We now define an
identity annotation that is used to construct recursive datatypes that do not
have any associated functionality.

> newtype Id f ix = Id { unId :: f ix }

The |Out| and |In| instances for the |Id| type solely unwrap and wrap the |Id|
constructor and do not perform any side effects. Because the associated context
is irrelevant we use the |Identity| monad, that effectively yields pure code.

> instance Traversable f => In Id f Identity where
>   inA = return . Id
>
> instance Traversable f => Out Id f Identity where
>   outA = return . unId

An recursive structure annotated with the identity annotation is isomorphic to
an unannotated structure. Using the |fullyOut| function we can make a pure
function that converts an structure annotated with identity annotations to an
unannotated structure:

> fullyOutId :: Traversable f => FixA Id f -> Fix f
> fullyOutId = runIdentity . fullyOut


