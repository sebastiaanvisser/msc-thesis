%include polycode.fmt
%include thesis.fmt
%include forall.fmt

%if False

> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE
>     EmptyDataDecls
>   , GADTs
>   , KindSignatures
>   , RankNTypes
>   , FlexibleInstances
>   , FlexibleContexts
>   , TypeOperators
>   , TypeFamilies
>   , MultiParamTypeClasses
>   , GeneralizedNewtypeDeriving
>   #-}
> module HigherOrder where

> import Control.Applicative
> import Control.Monad
> import Data.Monoid

%endif

\section{Moving away from regular datatypes}

In the previous chapter we have shown how to build a generic storage framework
for recursive data structures. This framework only works for regular datatypes
\docite{regular datatypes}, types in which the recursive positions can only
refer to the exact same type again. 

Mutually recursive datatypes, or indirect recursive dataypes, are types in which
the recursion positions refer to other datatypes that directly or indirectly
refer back to the original. A common example of a mutual recursive datatype is a
syntax tree containing both expressions and declarations. \docite{multirec
example}.

> data Expr =
>      Const  Int
>   |  Add    Expr
>   |  Mul    Expr
>   |  EVar   String
>   |  Let    Expr
>
> data Decl =
>      String := Expr
>   |  Seq Decl Decl

Nested datatypes are parametrized recursive datatypes that have recursive
position in which the type parameter changes. An example of a nested datatype
is the \emph{perfect tree}, a tree that grows in size exponentially at every
recursive position. \docite{initial}

> data PerfectTree a =
>     PerfLeaf a
>  |  PerfNode (PerfectTree (a, a))

Generalized algebraic datatypes (GADTs) have the possibility to add extra
indices to the datatype. These indices can be used to encode type invariants
and limit the recursive nesting. GADTs with indices are called indexed
datatypes or higher order datatypes. A common example of an indexed datatype is
the |Vector| type, a sequence that encodes its length in the type. The |Vector|
datatype uses Peano style natural numbers at the type level, which is either a
zero or a successor of another natural number. For conciseness natural number
at the type level are typeset as true number.

> data Zero
> data Succ a

Just like Haskell lists, the |Vector| type has two constructors, |Nil| and
|Cons|. The |Nil| can be used to construct an empty |Vector| with index 0, the
|Cons| can be used to append one value to the |Vector| increasing its length
with one.

> data Vector i a where
>   Nil   ::                     Vector Zero      a
>   Cons  :: a -> Vector i a ->  Vector (Succ i)  a

%if False

> type One    = Succ Zero
> type Two    = Succ (Succ Zero)
> type Suc2 a = Succ (Succ a)

%endif

These three examples show that regular datatypes are not the only types
commonly used container data structures.  Both mutually recursive datatypes,
nested datatypes and indexed datatypes are not regular datatypes and cannot be
used with the storage framework from the previous chapters. In this chapter we
will extend the framework to allow using indexed datatypes, including indexed
GADTs, as the persistent container datatypes. The global architecture of the
system remains the same, but we have to rewrite almost all individual
components to allow the use of indexed datatypes.

In this chapter we will only focus on explicitly indexed datatypes using GADTs,
we will not discuss mutually recursive datatypes and nested datatypes. However,
we will as an example show how to rewrite a nested finger tree\cite{fingertree}
datatype as one single indexed GADT, keeping the same invariants as the nested
type.  Our assumption \todo{no assumption} \docite{foundationsfor} is that most
if not all nested datatypes can be rewritten as an explicitly indexed GADT,
preserving all invariants. In the paper \emph{Generic programming with fixed
points for mutually recursive datatypes} Rodriguez et al. show how to encode a
collection of mutually recursive datatypes as an indexed GADT in order to
perform generic programming over this GADT. This observation indicates that the
persistent system for indexed datatypes we are about to introduce will also
work for mutually recursive datatypes, although a slightly different encoding
has to used. \todo{some future work?}

\section{Higher order fixed points}

We start at the same place as the framework for regular recursive datatypes, we
introduce an annotated fixed point combinator. The higher order fixed point
combinator is very similar to the regular fixed combinator although it is
parametrized with an additional type index called |ix|. The |ix| is used as an
additional type variable to the container structure |f|, either directly or
indirectly through the annotation type |a|. In this chapter we will commonly
prefix types and function names with an additional |H| to make clear we are
dealing with the higher order variant of the type or function.

> data HFixA a f ix
>   =  HInA  { houta  :: a  f (HFixA a f) ix }
>   |  HInF  { houtf  ::    f (HFixA a f) ix }

At first sight this higher order version looks very similar to the regular
fixed combinator, the only obvious addition is the index parameter |ix|. But
due to this index the kinds of the other type variables become more
complicated. Assuming a kind |*| for |ix|, the type kind of variable |f|
changes from |* -> *| to |(* -> *) -> * -> *|, which makes the kind of the
type variable |a| grow from |(* -> *) -> * -> *| to |((* -> *) -> * -> *) ->
(* -> *) -> * -> *|. This has a lot of impact on the rest of our framework,
introducing one additional type index forces us to changes almost all the types
in our system. We start by defining the indexed identity annotation.

> newtype HId f b ix = HId { unHId :: f b ix }

As the type shows, the identity annotation also takes the additional type index
|ix| and uses this to parametrize the indexed functor |f|.

Now we can create a annotation free fixed point combinator again by
parametrizing the annotated fixed point with the identity annotation.

> type HFix f ix = HFixA HId f ix

\section{Finger Tree}

To illustrate the usage of the higher order fixed point combinator we will in
this chapter model a finger tree data type as a indexed GADT. The finger tree
is a purely functional data structure that can be used to model an abstract
sequence with very interesting runtime behaviour. In the paper \emph{Finger
trees: a simple general-purpose data structure}\cite{fingertree} Hinze and
Paterson show how to implement a finger tree in Haskell using a nested
datatype. The datatypes the authors describe looks more or less\footnote{In
their paper Hinze and Paterson use a slightly less constraint type in which
they encode the digit as a Haskell list datatype, with the implicit invariant
this list has length greater than or equal to 1 and less than or equal to 4.}
like the following.

\begin{spec}
data Node  a = Node2 a a  | Node3 a a a

data Digit a = Digit1 a | Digit2 a a | Digit3 a a a | Digit4 a a a a

data FingerTree a =
  Empty | Single a | Deep (Digit a) (FingerTree (Node a)) (Digit a)
\end{spec}

The FingerTree datatype can be seen as a spine structure containing zero, one
or two 2-3 trees per spine node. In this example 2-3 trees are trees that
branch with a factor of 2 or a factor of 3 in their recursive positions. To
allow a bit more flexibility at the root nodes of the 2-3 trees the |Digit|
type is used to allow a branching factor between 1 and 4. Hinze and Paterson
show that flexing the branching factor on the top-level of the 2-3 trees makes
it easier to write algorithms and efficiency proofs for finger trees but proof
this does not negatively influence the asymptotic running time of the
algorithms. Finger trees containing zero or one 2-3 trees must be created with
the |Empty| or |Single| constructors and have only one spine node. Larger
finger tree containing more spine nodes are constructed using the |Deep|
constructor. The |Deep| constructor takes two 2-3 trees -- with a |Digit| root
node -- and takes one extra spine node for the rest of the tree. At every
recursive position the value type changes by surrounding it with one |Node|
level, this implicitly encodes the size of the 2-3 trees at every level of the
spine grows.

The paragraph above shows that the nested finger tree datatype encodes some
important invariants about the structure of finger trees in the type. Before we
will try to describe the same structure as an indexed GADT we will extract a
specification from the nested datatype definition above.

\todo{make a picture}

\begin{enumerate}

\item A finger tree contains spine nodes, digit nodes, 2-3 nodes and values.

\item A spine node is either an empty node, a single node containing one
2-3 tree or a deep containing two 2-3 trees and a tail finger tree. The spine
nodes form a linearly linked list.

\item The first spine node in a non singleton finger tree only contains two
single level 2-3 trees. The second spine node only contains two two-level 2-3
trees, the third level stores two three-level 2-3 trees and so on.  A quick
calculation shows that every node in a finger tree, except for the last
terminating node, contains a maximum of $2 * 4 * 3^n$ values, where |n| is the
index of the node.

\item A 2-3 tree is either a single value of type |a|, a branch node with 2 or
3 sub trees or a root node which is called a digit.

\item Digits have a branching factor of 1, 2, 3 or 4. Digits are the root nodes
of 2-3 tree and can only contain nodes or values.

\end{enumerate}

We can now try to capture the specification including the invariants between
the different nodes in an indexed GADT. Our finger tree GADT will contain a
single product type as the index. The product contains two components, one for
the kind of finger tree node and one for the encoding of the tree depths. We
first define three phantom types indices to indicate spine node, a digit node
and a 2-3 tree node.  

> data Sp
> data Dg
> data Nd

The depth encoding will be written down as a subscript type level natural
number, similar to the index of the |Vector| datatype above.

> type SpI i  = (Sp, i)
> type DgI i  = (Sp, i)
> type NdI i  = (Sp, i)

Now we can define the finger tree GADT that encodes all the invariants from the
specification. 

> data Tree (a :: *) (f :: * -> *) :: * -> * where
>   Empty   ::                                                               Tree a f (SpI (Succ i))
>   Single  :: f (DgI (Succ i))                                          ->  Tree a f (SpI (Succ i))
>   Deep    :: f (DgI (Succ i)) -> f (SpI (Suc2 i)) -> f (DgI (Succ i))  ->  Tree a f (SpI (Succ i))
>
>   Digit1  :: f (NdI i)                                                 ->  Tree a f (DgI (Succ i))
>   Digit2  :: f (NdI i) -> f (NdI i)                                    ->  Tree a f (DgI (Succ i))
>   Digit3  :: f (NdI i) -> f (NdI i) -> f (NdI i)                       ->  Tree a f (DgI (Succ i))
>   Digit4  :: f (NdI i) -> f (NdI i) -> f (NdI i) -> f (NdI i)          ->  Tree a f (DgI (Succ i))
>
>   Node2   :: f (NdI i) -> f (NdI i)                                    ->  Tree a f (NdI (Succ i))
>   Node3   :: f (NdI i) -> f (NdI i) -> f (NdI i)                       ->  Tree a f (NdI (Succ i))
>   Value   :: a                                                         ->  Tree a f (NdI Zero)

The finger tree has an explicit type parameter |f| for the recursive positions,
just like the binary tree example for the regular recursive datatypes. Both the
finger tree datatype as the type variable for the recursive positions have kind
|* -> *| and expect a type index. Every constructor produces a finger tree with
its own specific index and can enforce an constraint between this index and the
index of the recursive positions.

The definition shows that the |Empty|, |Single| and |Deep| constructors all
create spine nodes with a non-zero depth index. A |Single| spine node contains
a |Digit| root node of a 2-3 tree with exactly the same non-zero depth as the
spine node encodes, this is enforced in the index parameter for the recursive
structure |f|. The |Deep| spine node contains two |Digit| root nodes of 2-3
tree, also with the same depth index. The tail finger tree stored in the |Deep|
constructor's second argument must also be a spine node with a depth index
one larger than the current spine node. This enforces the tail to store more
values than the head of a finger tree as specified in point 3 of the
specification.

There are four different constructors for creating digit nodes, one for each
arity. The digit constructors all take sub trees with the |Nd| index at the
recursive positions, which can mean both 2-3 branching nodes or plain values.
The digit nodes all a depth index one larger than their sub trees.

Like digit nodes the 2-3 node constructors |Node2| and |Node3| take sub trees
with the |Nd| index at the recursive positions, which can mean a 2-3 branching
node again or plain values. Also, the depth index one larger than their sub
trees. A value is special kind of node that always has index zero. It contains
a value of type |a| which is the value type parameter of the finger tree
datatype.

This indexed GADT encodes all the properties from our specification into a
single structure. We can now use the higher order fixed point combinator to tie
the knot and create a recursive data structure again by filling in the |f| type
parameter.

> type FingerTreeA a b   = HFixA a (Tree b) (SpI One)

So, an annotated finger tree with annotation type |a| that stores values of type
|b|. A finger tree root node always is a spine node with depth index one. Now
we can also define a bunch of additional type synonyms to simplify working with
our finger trees.

> type Node   a b i  = HFixA a (Tree b)  (NdI i)
> type Value  a b    = HFixA a (Tree b)  Zero
> type Digit  a b i  = HFixA a (Tree b)  (DgI (Succ i))
> type Spine  a b i  = HFixA a (Tree b)  (SpI (Succ i))
                     
\section{Higher order traversals}

In the section about fixed point combinators for regular datatypes we have
derived |Functor|, |Foldable| and |Traversable| instances for our binary tree
example datatype. These traversal functions allowed us to perform a simple form
of generic programming over our datatypes which is an essential part of our
generic persistence framework. Unfortunately, these type classes cannot be used
for our higher order datatypes like our finger tree GADT. In order to perform
generic traversal over indexed datatypes we have to introduce higher order
variant of these three type classes.

\subsection{Higher order Functor}

In their paper \emph{Initial Algebra Semantics is Enough!}\cite{initial} Ghani
and Johann describe how to create a type class for higher order functors. These
functors work on indexed datatypes and define a natural transformation that
must work for all type indices.

> class HFunctor h where
>   hfmap :: (forall ix. a ix -> b ix) -> forall ix. h a ix -> h b ix

We define a derived higher order functor type class |PFunctor| that gets an
additional proof object |phi|, that proves the index is an inhabitant of a
specific \emph{family}. This allows us to write functors instances that work
not for all type indices but for an explicitly limited set. \todo{explain}

> class PFunctor phi h where
>   pfmap :: (forall ix. phi ix -> a ix -> b ix) -> forall ix. phi ix -> h a ix -> h b ix


\subsubsection{Finger tree instance for PFunctor}

Before we can create a |PFunctor| instance for the finger tree datatype the
typeclass forces us to make explicit the index family we want to reason about.
We construct a GADT that serves as a proof object that proves that a certain
index is actually a possible index for our |Tree| GADT. We will indicate proof
types and proof values with a postfix $\phi$.

> data TreePhi :: * -> * where
>   SpPrf   :: NatPrf c ->  TreePhi (SpI (Succ c))
>   DgPrf   :: NatPrf c ->  TreePhi (DgI (Succ c))
>   NdPrf   :: NatPrf c ->  TreePhi (NdI (Succ c))
>   NdZPrf  ::              TreePhi (NdI Zero)

This proof type can be used to show that for every natural number there is a
\emph{spine}, \emph{digit} and \emph{node} index for the successor of that
natural number. For this proof object we also need a natural number proof.

> data NatPrf :: * -> * where
>   ZeroP  ::              NatPrf Zero
>   SuccP  :: NatPrf n ->  NatPrf (Succ n)

This datatype can be used to constructor value for every natural number index.
Note that the structure of the proof indices is very similar to the structure
of the finger tree indices. No we can create a |PFunctor| instance in which we
can pattern match on both the proof object and the finger tree constructor.
Because the type signature of |pfmap| parametrizes the proof ($\phi$) and the
constructor ($a$) with the same index, the compiler should be able to see
that there are only a limited set of proof/constructor combinations possible.

> instance PFunctor TreePhi (Tree a) where
>   pfmap _ (SpPrf  _)          (Empty         )  = Empty
>   pfmap f (SpPrf  p)          (Single a      )  = Single (f (DgPrf p) a)
>   pfmap f (SpPrf  p)          (Deep   a c b  )  = Deep   (f (DgPrf p) a) (f (SpPrf (SuccP p)) c) (f (DgPrf p) b)
>   pfmap f (DgPrf  (SuccP p))  (Digit1 a      )  = Digit1 (f (NdPrf p) a)
>   pfmap f (DgPrf  (SuccP p))  (Digit2 a b    )  = Digit2 (f (NdPrf p) a) (f (NdPrf p) b)
>   pfmap f (DgPrf  (SuccP p))  (Digit3 a b c  )  = Digit3 (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c)
>   pfmap f (DgPrf  (SuccP p))  (Digit4 a b c d)  = Digit4 (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c) (f (NdPrf p) d)
>   pfmap f (NdPrf  (SuccP p))  (Node2  a b    )  = Node2  (f (NdPrf p) a) (f (NdPrf p) b)
>   pfmap f (NdPrf  (SuccP p))  (Node3  a b c  )  = Node3  (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c)
>   pfmap f (DgPrf  ZeroP)      (Digit1 a      )  = Digit1 (f NdZPrf a)
>   pfmap f (DgPrf  ZeroP)      (Digit2 a b    )  = Digit2 (f NdZPrf a) (f NdZPrf b)
>   pfmap f (DgPrf  ZeroP)      (Digit3 a b c  )  = Digit3 (f NdZPrf a) (f NdZPrf b) (f NdZPrf c)
>   pfmap f (DgPrf  ZeroP)      (Digit4 a b c d)  = Digit4 (f NdZPrf a) (f NdZPrf b) (f NdZPrf c) (f NdZPrf d)
>   pfmap _ NdZPrf              (Value  a      )  = Value a

To allow the |pfmap| function for this instance to apply the map function |f|
to the sub trees we need to construct an appropriate proof again. We can do
this by unpacking and repack the proof as input using the right constructor
and pass it to the function |f|. This works out exactly right because the proof
type follows the same index structure as our finger tree.

This |PFunctor| instance allows us to map a function over one level of
recursive positions of the finger tree GADT. The we can use the proof object to
distinguish between different positions in the structure. The |PFunctor|
instances will form the basis of generic traversals over higher order
datatypes with restricted families of indices.

\subsection{Higher order Foldable}

\todo{Skip this one? We do not really use |Foldable| anyway.}

\subsection{Higher order Traversable}

Besides the higher order |Functor| instance we can also make a higher order
|Traversable| instance, allowing us to perform effectful traversals. First we
define a |PTraversable| type class similar to the |PFunctor| type class, we use
the same proof object $\phi$ to restrict the family of indices.

> class PFunctor phi h => PTraversable phi h where
>   ptraverse  ::  Applicative f
>              =>  (  forall ix. phi ix ->    a ix -> f (   b ix))
>              ->     forall ix. phi ix -> h  a ix -> f (h  b ix)

So if we are provided an effecful computation for the element type |a ix| in
some |Applicative| -- or possibly monadic -- context |f|, the |ptraverse|
function should be able to apply this to all elements of the structure
|h a ix|. The |PTraversable| instance for the finger tree GADT is follows the
same pattern as the regular |Traversable| instance, although we have to pattern
match on- and recursively supply the proof objects, just like the |PFunctor|
instance. 

> instance PTraversable TreePhi (Tree a) where
>   ptraverse f phi h =
>     case (phi, h) of
>       (SpPrf _,          Empty)           -> (| Empty                                                                      |)
>       (SpPrf p,          Single a)        -> (| Single (f (DgPrf p)  a)                                                    |)
>       (SpPrf p,          Deep   a c b)    -> (| Deep   (f (DgPrf p)  a) (f (SpPrf (SuccP p)) c)           (f (DgPrf p) b)  |)
>       (DgPrf (SuccP p),  Digit1 a)        -> (| Digit1 (f (NdPrf p)  a)                                                    |)
>       (DgPrf (SuccP p),  Digit2 a b)      -> (| Digit2 (f (NdPrf p)  a) (f (NdPrf p) b)                                    |)
>       (DgPrf (SuccP p),  Digit3 a b c)    -> (| Digit3 (f (NdPrf p)  a) (f (NdPrf p) b)  (f (NdPrf p) c)                   |)
>       (DgPrf (SuccP p),  Digit4 a b c d)  -> (| Digit4 (f (NdPrf p)  a) (f (NdPrf p) b)  (f (NdPrf p) c)  (f (NdPrf p) d)  |)
>       (NdPrf (SuccP p),  Node2  a b)      -> (| Node2  (f (NdPrf p)  a) (f (NdPrf p) b)                                    |)
>       (NdPrf (SuccP p),  Node3  a b c)    -> (| Node3  (f (NdPrf p)  a) (f (NdPrf p) b)  (f (NdPrf p) c)                   |)
>       (DgPrf ZeroP,      Digit1 a)        -> (| Digit1 (f NdZPrf     a)                                                    |)
>       (DgPrf ZeroP,      Digit2 a b)      -> (| Digit2 (f NdZPrf     a) (f NdZPrf    b)                                    |)
>       (DgPrf ZeroP,      Digit3 a b c)    -> (| Digit3 (f NdZPrf     a) (f NdZPrf    b)  (f NdZPrf c)                      |)
>       (DgPrf ZeroP,      Digit4 a b c d)  -> (| Digit4 (f NdZPrf     a) (f NdZPrf    b)  (f NdZPrf c)     (f NdZPrf d)     |)
>       (NdZPrf,           Value  a)        -> (| (Value a)                                                                  |)

The instance uses idiom brackets for the effectful computations. With the both
the higher order functor and traversable instances for our finger tree GADT, we
can now start writing generic recursive traversals.

\section{In and Out type classes}

In the our generic annotation framework for regular datatypes we have created
three type classes to associate custom functionality with wrapping and
unwrapping annotations. We have to do the same for our higher order annotation
framework, but we cannot use the existing type classes due a clear type
mismatch. In this chapter we extend the |AnnI| and |AnnO| type classes to
work with indexed data types and show how make instances for the higher order
identity and debug annotations.

We first define the three type signatures, for the higher order query, producer
and modifier functions. The type signatures already make clear the difference
between annotation working on indexed types and our previous annotations
working on regular recursive data structures.

> type HIn     a h phi m  =    forall ix. phi ix ->  h (  HFixA a h) ix ->  m       (HFixA a h  ix)
> type HOut    a h phi m  =    forall ix. phi ix ->       HFixA a h  ix ->  m (  h  (HFixA a h) ix)
> type HInOut  a h phi m  = (  forall ix. phi ix ->  h (  HFixA a h) ix ->       h  (HFixA a h) ix)
>                         ->   forall ix. phi ix ->       HFixA a h  ix ->  m       (HFixA a h  ix)

All three function types take an annotation |a|, a higher order recursive
structure |h| with explicit recursive positions, a index family proof object
$\phi$ and some monadic context |m|. The |HIn| type describes a producer
function that takes a single unannotated node with fully recursive sub
structures and wraps it with some annotation, possibly inside some effectful
context. The functions can restrict the family of indices with the proof term
$\phi$. The |HOut| type describes a query function that performs the dual task
of the producer. It takes a fully annotated tree and unwraps the root
annotation, returning a single unannotated node with fully annotated sub
structures. The modifier function simply combines the query and producer
functions in one step.

Now we introduce the three type classes that implement respectively a producer,
a query and a modifier function for some annotation |a|, for some recursive
structure |h|, for some index family $\phi$ and in some context |m|. These for
type variables in the type class make it possible to get fine grained control
over when which annotation should be applied in what part of a recursive
structure. However, in this document we will keep both |h| and $\phi$ polymorph
and will only specialize based on some |a| running in a fixed context |m|.

> class (Applicative m, Monad m) => AnnO a h phi m where
>   annO :: HOut a h phi m
> 
> class (Applicative m, Monad m) => AnnI a h phi m where
>   annI :: HIn a h phi m

Besides the additional proof type $\phi$ the types of the annotation type
classes are very similar to the ones for regular recursive data structures.
Again, the modifier function has a default implementation in terms of the query
and producer function.

> class (AnnO a h phi m, AnnI a h phi m) => AnnIO a h phi m where
>   annIO :: HInOut a h phi m
>   annIO f phi = annI phi . f phi <=< annO phi

The instances for the |HId| annotation are as simple as wrapping and unwrapping
the constructor.

> instance (Applicative m, Monad m) => AnnO HId h phi m where
>   annO _ (HInA (HId f))  = return f
>   annO _ (HInF      f )  = return f
> 
> instance (Applicative m, Monad m) => AnnI HId h phi m where
>   annI _ = return . HInA . HId
> 
> instance (Applicative m, Monad m) => AnnIO HId h phi m

Although the types have changed the annotation framework is very similar to the
one for regular recursive data structures. We can now use these type classes to
implement a paramorphism for indexed datatypes.

\section{Higher order annotated paramorphism}

In this section we introduce paramorphic traversals for higher order datatypes.
In order to express the algebras we define the higher order sum and product
types, similar to the Haskell |Either a b| and |(a, b)| (tuple) types, but with an
additional type index.

%if False

> infixl 6 :+:
> infixl 7 :*:

%endif

> data (f :+: g) ix = L (f ix) | R (g ix)

> data (f :*: g) ix = (:*:) { hfst :: f ix, hsnd :: g ix }

Using the product type we can construct the higher order paramorphic algebra.
Like the algebra for regular datatypes, this algebra should be able to
destruct one node with the recursive results in the recursive positions to a
some result value. The algebra can use the proof object as a restriction on the
index family. Both the input structure |f| and the output structure |g| are
indexed with the same index type |ix|. Because this is a paramorphic algebra,
the |f| structure contains both the recursive results and the fully annotated
sub structures as well.

> type HPsiA a phi f g = forall ix. phi ix -> f (HFixA a f :*: g) ix -> g ix

Now we define the higher order paramorphic traversal that uses an algebra to
recursively destruct the structure |HFixA a f ix| into a value of type |g ix|.
Because this is an annotated paramorphism the traversal uses the |annO|
method from the |AnnO| type class to unwrap the annotation, possibly in an
effecful context |m|.

> hparaMA
>   :: (AnnO a f phi m, PTraversable phi f)
>   => HPsiA a phi f g -> phi ix -> HFixA a f ix -> m (g ix)
> hparaMA psi phi = return . psi phi <=< ptraverse (\p x -> (| (x :*:) (hparaMA psi p x) |)) phi <=< annO phi

\todo{not laziness, not applicative, future work}

\subsection{Contains, Sum and Product}

To illustrate the usage of the higher order paramorphism we will define four
example algebras for finger tree datatype. All four algebras will be defined in
terms of one generic algebra that converts all value in a finger tree into some
monoid value and appends these together using the |`mappend`| operator.

The monoid type we use as the result type will be a plain type without any
indices. Because the algebra forces the result type to have an index as well,
we have to explicitly ignore the index when dealing with simple Haskell types.
We do this by introducing the constant functor, that accepts a value type and
an index type, but only uses the value type and ignore the index type.

> newtype K h a = K { unK :: h }

We can now create a generic fold algebra that returns some monoid |m| in the
constant functor.

> foldmAlg :: Monoid m => (b -> m) -> HPsiA a phi (Tree b) (K m)
> foldmAlg f _ h =
>   case h of
>     Empty            -> K mempty
>     Single a         -> K (g a)
>     Deep   a b c     -> K (g a `mappend` g b `mappend` g c)
>     Digit1 a         -> K (g a)
>     Digit2 a b       -> K (g a `mappend` g b)
>     Digit3 a b c     -> K (g a `mappend` g b `mappend` g c)
>     Digit4 a b c d   -> K (g a `mappend` g b `mappend` g c `mappend` g d)
>     Node2  a b       -> K (g a `mappend` g b)
>     Node3  a b c     -> K (g a `mappend` g b `mappend` g c)
>     Value  a         -> K (f a)
>  where g = unK . hsnd

The first parameter of this algebra is a function that converts the values
stored in the finger tree into some type |m| for which there is a |Monoid|
instance available. Because this algebra is a paramorphic algebra the function
can use both the recursive results and the original sub structures. This
algebra only uses the recrusive results, this actually makes it a
\emph{catamorphism}. The paramorphism also allows us to add an index type to
the result value, because we do not use this we ignore the index using the |K|
constructor. Unpacking the recursive result from the tuple and from the |K|
type is done with the helper function |g|.
  
The |Monoid| type class allows us to be generic in the type we want to fold. By
specializing the value type to |Int| and the output type to |Sum Int| or
|Product Int| \footnote{The |Sum| and |Product| newtypes can be found in
Haskell's |Data.Moinoid| package. They are used to specialize the way the
|`mappend`| operators combines two numeric values, either with |+| or with
|*|.} we can create two algebras that compute the sum and the product of a
finger tree containing integers.

> sumAlg :: HPsiA a phi (Tree Int) (K (Sum Int))
> sumAlg = foldmAlg Sum
>
> productAlg :: HPsiA a phi (Tree Int) (K (Product Int))
> productAlg = foldmAlg Product

Creating a fold algebra that concatenates all strings we can simply exploit the
default monoid instances for lists and parametrize |foldmAlg| with the identity
function.

> concatAlg :: HPsiA a phi (Tree String) (K (String))
> concatAlg = foldmAlg id

The last example is a containment check that uses an equality check in
combination with the |Any| monoid wrapper to check whether is certain value
exists in the finger tree sequence.

> containsAlg :: Eq b => b -> HPsiA a phi (Tree b) (K Any)
> containsAlg v = foldmAlg (Any . (==v))

These four algebras can now be lifted to true annotated traversals using the
|hparaMA| function. Note that in all four cases we supply an index proof to
paramorphism, the proof object contains the same index as the root of our
finger tree, which is |SpPrf ZeroP|. After computing the result we unpack it
from the constant functor |K| and from the monoid wrapper when needed.

> sum :: AnnO a (Tree Int) TreePhi m => FingerTreeA a Int -> m Int
> sum h = (| (getSum . unK) (hparaMA sumAlg (SpPrf ZeroP) h) |)

> product :: AnnO a (Tree Int) TreePhi m => FingerTreeA a Int -> m Int
> product h = (| (getProduct . unK) (hparaMA productAlg (SpPrf ZeroP) h) |)

> concat :: AnnO a (Tree String) TreePhi m => FingerTreeA a String -> m String
> concat h = (| unK (hparaMA concatAlg (SpPrf ZeroP) h) |)

> contains :: Eq b => AnnO a (Tree b) TreePhi m => b -> FingerTreeA a b -> m Bool
> contains v h = (| (getAny . unK) (hparaMA (containsAlg v) (SpPrf ZeroP) h) |)

These four algebras show that it does take that much to implement simple
catamorphisms that compute values of simple Haskell types over indexed data
structures. In the next section we show a more complex example, the |cons|
function that append one item to the beginning of the finger tree sequence.

\section{Cons}

Two of the basic operations on finger trees as described in the paper by Hinze
and Paterson are the |cons| and the |snoc| functions. These functions append or
prepend one item to the sequence. Two seemingly simple functions, thought they
are both a bit involved because they have to preserve the nested structure of
the finger tree. In this section we briefly show what is needed to write a
paramorphic algebra for the cons function on our finger tree GADT. We will only
give the type signature and not the actual implementation.

Writing down the correct type signature and using the final algebra to build an
actual |cons| function on annotated finger trees is a bit involved. We need to
construct a paramorphism that takes as input the original annotated finger tree
and computes a function that takes the node to append and results in the new
finger tree. We do need a lot of type level helpers to ensure the paramorphism
resulting in an append function conforms all the type level invariant encoded
in the GADT.

First we define two type families two compute the first and second type level
component from our indexed product type. Both type level families simply
project the left or right component and apply the original index to the result.

> type family Fst a :: *
> type instance Fst ((a :*: b) ix) = a ix
  
> type family Snd a :: *
> type instance Snd ((a :*: b) ix) = b ix

The append function that is computed by the paramorphic algebra needs itself to
reason about the index types, therefore we need a higher order function type
that distributes an index to both the co- and contra variant positions.

> infixr 1 :->
> data (:->) a b ix = F { unF :: a ix -> b ix }

The inductive structure of the GADT forces us to reason about the numeric index
relation of two connected nodes. Computing the successor of a type level
natural number can be done using the successor (written down as |Succ n|)
datatype. We now introduce a predecessor type family (written down as |Pred n|)
that decrements any non-zero type level natural with one.

> type family Pred a
> type instance Pred (Succ c) = c

Now we introduce two type level functions -- encoded as datatypes -- that allow
us to change the GADT index. The |N| datatype takes a container datatype and an
index and returns the same type but forces it to be a node type by using the
|Nd| phantom type as the first component of index tuple. A |Maybe| type is to
indicate a possible empty result. The |N| data function keeps the depth index
intact. The |D| type level function encodes a similar pattern but decrements
the depth index by one using the predecessor type family defined above.

> newtype N  f ix = N  { unN    :: Maybe (f (Nd, Snd ix))     }
> newtype D  f ix = D  { unDec  :: f (Fst ix, Pred (Snd ix))  }

We now have all the components to write down the paramorphic algebra for the
cons function. The type is rather complicated because it encodes all invariants
involved when appending an item to the head of the sequence.

> consAlg :: tree ~ HFixA a (Tree b) => HPsiA a TreePhi (Tree b) (N (D tree) :-> tree :*: N tree)

Let try to explain what this type means. 

To simplify the type signature a type variable |tree| is defined as a shortcut
for a fully annotated finger tree structure with value of type |b|.

We have a paramorphic algebra that takes an annotated finger tree as input
and computes a function from a finger tree \emph{node} to a product of some
finger tree and some finger tree \emph{node}. 

The result is a product type because it returns both the new finger tree
and possibly a overflow node that needs to be inserted one level deeper. So, if
we push a node to the head of sequence it gets inserted in the left 2-3 tree of
the first spine node, when this 2-3 tree is already full one node or sub-tree
is selected to be cons'ed to a spine node one deeper. 

The input node always has a depth index one less than depth index of the
sub-tree being traversed by the algebra, indicated by the |D| type.

The result tree index is unaffected, this makes sense: cons'ing a node to a
finger tree should return a finger tree with the same index.

Both the input and overflow part of the output of the computed functions are
nodes, because only value can be inserted inserted into a finger tree and only
2-3 trees can overflow to a deeper level. The |N| type also encodes the
optionality using the |Maybe| type, this allows the algebra to stop the
insertion when no node overflows. Hinze and Paterson proof that overflowing
happens rarely and the amortized time for cons'ing a node is still $O(1)$.

One of the constraints of the cons algebra is that we can only insert nodes
with a depth index one less than the tree it gets appended to. At the top level
a finger tree is always a spine node with depth index one, as can be seen in the
|FingerTree| type synonym. This means we can only insert nodes with depth index
zero into a top level finger tree, as expected only |Value| nodes have index
zero. The |D| type family can only be applied to non-zero indices, to proof to
the compiler our input finger tree always has a non-zero index we parametrize
the algebra with our |TreePhi| proof object.

This example shows it is still possible to abstract away from recursion when
writing operations over indexed datatypes. Higher order algebras can be written
that preserve all invariants encoded in a GATD. However, we cannot help to
conclude that \emph{it is very hard to do so}. \todo{cry a bit about the
complexity}

%if False

> consAlg (SpPrf _) = undefined

%endif

\section{HPersistent}

\section{Running example!}

