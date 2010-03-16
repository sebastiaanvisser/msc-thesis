%include polycode.fmt
%include thesis.fmt
%include forall.fmt

%if False

> {-# LANGUAGE
>     EmptyDataDecls
>   , GADTs
>   , KindSignatures
>   , RankNTypes
>   , MultiParamTypeClasses
>   #-}
> module HigherOrder where

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
recursive position.

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
> 
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
type.  Our assumption is that most if not all nested datatypes can be rewritten
as an explicitly indexed GADT, preserving all invariants. In the paper
\emph{Generic programming with fixed points for mutually recursive datatypes}
Rodriguez et al. show how to encode a collection of mutually recursive
datatypes as an indexed GADT in order to perform generic programming over this
GADT. This observation indicates that the persistent system for indexed
datatypes we are about to introduce will also work for mutually recursive
datatypes, although a slightly different encoding has to used. \todo{some
future work?}

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

First we construct a GADT that server as a proof object that proves that a certain index is actually a valid index for 

> data TreePhi :: * -> * where
>   SpPrf   :: NatPrf c ->  TreePhi (SpI (Succ c))
>   DgPrf   :: NatPrf c ->  TreePhi (DgI (Succ c))
>   NdPrf   :: NatPrf c ->  TreePhi (NdI (Succ c))
>   NdZPrf  ::              TreePhi (NdI Zero)

> data NatPrf :: * -> * where
>   ZeroP  ::              NatPrf Zero
>   SuccP  :: NatPrf n ->  NatPrf (Succ n)

\begin{spec}
instance PFunctor TreePhi (Tree a) where
  pfmap _ (SpPrf _)         (Empty         ) = Empty
  pfmap f (SpPrf p)         (Single a      ) = Single (f (DgPrf p) a)
  pfmap f (SpPrf p)         (Deep   a c b  ) = Deep   (f (DgPrf p) a) (f (SpPrf (SuccP p)) c) (f (DgPrf p) b)
  pfmap f (DgPrf (SuccP p)) (Digit1 a      ) = Digit1 (f (NdPrf p) a)
  pfmap f (DgPrf (SuccP p)) (Digit2 a b    ) = Digit2 (f (NdPrf p) a) (f (NdPrf p) b)
  pfmap f (DgPrf (SuccP p)) (Digit3 a b c  ) = Digit3 (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c)
  pfmap f (DgPrf (SuccP p)) (Digit4 a b c d) = Digit4 (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c) (f (NdPrf p) d)
  pfmap f (NdPrf (SuccP p)) (Node2  a b    ) = Node2  (f (NdPrf p) a) (f (NdPrf p) b)
  pfmap f (NdPrf (SuccP p)) (Node3  a b c  ) = Node3  (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c)
  pfmap f (DgPrf ZeroP)     (Digit1 a      ) = Digit1 (f NdZPrf a)
  pfmap f (DgPrf ZeroP)     (Digit2 a b    ) = Digit2 (f NdZPrf a) (f NdZPrf b)
  pfmap f (DgPrf ZeroP)     (Digit3 a b c  ) = Digit3 (f NdZPrf a) (f NdZPrf b) (f NdZPrf c)
  pfmap f (DgPrf ZeroP)     (Digit4 a b c d) = Digit4 (f NdZPrf a) (f NdZPrf b) (f NdZPrf c) (f NdZPrf d)
  pfmap _ NdZPrf            (Value  a      ) = Value a
  pfmap _ _ _ = error "PFunctor TreePhi (Tree a): suppress warnings. "
\end{spec}




\subsection{Higher order Foldable}

\subsection{Higher order Traversable}

\section{Higher order annotated paramorpism}

  \subsection{laziness?}
  \subsection{applicative?}

\section{Higher order annotated apomorpism?}


