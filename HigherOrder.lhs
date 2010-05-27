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
>   , UndecidableInstances
>   #-}
> module HigherOrder where

> -- import Data.ByteString.Lazy
> import Data.Binary
> -- import Data.Binary.Get
> -- import Data.Binary.Put
> import Control.Applicative
> import Control.Monad
> import Heap
> import Storage
> import Data.Monoid
> import Prelude hiding (sum)

%endif

\chapter{Indexed Datatypes}
\label{chap:indexed}

In the previous chapters we have shown how to build a generic storage framework
for recursive data structures. This framework only works for regular datatypes,
types in which the recursive positions can only refer to the exact same type
again. The system does not work for any-non regular datatypes like mutually
recursive datatypes \cite{multirec}, nested datatypes \cite{nested} and
indexed datatypes like generalized algebraic datatypes or
GADTs \cite{foundationsfor}. In this section we show how to extend our
current system to work with non-regular datatypes. First we explore some
examples of non-regular datatypes.

\begin{itemize}

\item
\emph{Mutually recursive datatypes}, or \emph{indirect recursive datatypes},
are types in which the recursive positions refer to other datatypes that
directly or indirectly refer back to the original.  Rodriguez et
al. Rodriguez et al. \cite{multirec} show an interesting example of a mutual recursive datatype.
They present a syntax tree containing both expressions and declarations.

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

In this example the |Expr| datatype uses the |Decl| datatype and vice versa.

\item

\emph{Nested datatypes} are parametrized recursive datatypes that have one or
more recursive positions in which the type parameter changes. An example of a
nested datatype is the \emph{perfect tree} \cite{perfect}, a tree that grows in
size exponentially at every recursive position.

> data PerfectTree a =
>     PerfLeaf a
>  |  PerfNode (PerfectTree (a, a))

Because the |PerfectTree| uses a pair in the type index in the recursive
position, every deeper level contains twice as many elements as their previous
level.

\item

\emph{Generalized algebraic datatypes}, or \emph{GADTs}, have the possibility
to add extra indices to the datatype that can be specialized on a
per-constructor basis.  These indices can be used to encode type invariants can
limit the recursive nesting. GADTs with indices are called indexed datatypes or
higher order datatypes. A common example of an indexed datatype is the |Vector|
type, a sequence that encodes its length in the type. The |Vector| datatype
uses Peano style natural numbers at the type level, which is either a zero or a
successor of another natural number. For conciseness, natural number at the
type level are typeset as true numbers.

> data Zero
> data Succ a

Just like Haskell lists, the |Vector| type has two constructors: |Nil| and
|Cons|. The |Nil| can be used to construct an empty |Vector| with index 0, the
|Cons| can be used to prepend one value to a |Vector| increasing its length
with one.

> data Vector i a where
>   Nil   ::                     Vector Zero      a
>   Cons  :: a -> Vector i a ->  Vector (Succ i)  a

%if False

> type One    = Succ Zero
> type Two    = Succ (Succ Zero)
> type Suc2 a = Succ (Succ a)

%endif

\end{itemize}

These three examples show that regular datatypes are not the only commonly used
container data types.  Unfortunately, the generic programming techniques
introduced in the previous chapters do not allow mutually recursive datatypes,
nested datatypes and indexed datatypes to be used with our storage framework.
In this chapter we extend the framework to allow using indexed
datatypes, including indexed GADTs, as the persistent container datatypes.
The global architecture of the system remains the same, but, as we
see, we have to rewrite almost all individual components to allow the use
of indexed datatypes.

In this chapter we only focus on explicitly indexed datatypes using GADTs.
We do not discuss mutually recursive datatypes and nested datatypes.
However we show, as an example, how to rewrite a nested finger
tree \cite{fingertree} datatype to a single indexed GADT. The GADT has the
same invariants as the original nested finger tree type as presented by Hinze
et al.

Because generalized algebraic datatypes are a generalization of the algebraic
datatypes in Haskell, all nested datatypes can be written down as a GADT. By
making the nested behaviour of nested datatypes explicit using a type variable
in a GADT our framework can also be applied to other nested datatypes.

Rodriguez et al. \citet{multirec} show how to encode a family of mutually
recursive datatypes as an indexed GADT in order to perform generic programming
over this family. This observation indicates that the persistence framework for
indexed datatypes we are about to introduce also works for families of
mutually recursive datatypes. However, the families of datatypes have to be
written down in a slightly different encoding.

\section{Higher order fixed points}

We start at the same place as we did for the framework for regular recursive
datatypes: we introduce an annotated fixed point combinator. The higher order
fixed point combinator is similar to the regular fixed combinator although
it is parametrized with an additional type index called |ix|. The |ix| is used
as an additional type variable to the container structure |f|, either directly
or indirectly through the annotation type |a|. The higher order annotated fixed
point combinator is called |HFixA|.  In this chapter we commonly
prefix types and function names with an additional |H| or |h| to make clear we
are dealing with the higher order variant of the type or function.

> data HFixA a f ix
>
>   =  HInA  { houta  :: a  f (HFixA a f) ix }
>
>   |  HInF  { houtf  ::    f (HFixA a f) ix }

Now recall the original annotated fixed point combinator:

\begin{spec}
data FixA a f

  =  InA  { outa  :: a  f (FixA a f) }

  |  InF  { outf  ::    f (FixA a f) }
\end{spec}

At first sight, this higher order version looks similar to the regular
fixed combinator, the only obvious addition is the index parameter |ix|. But
because of to this index the kinds of the other type variables become more
complicated. Assuming a kind |*| for |ix|, the type kind of variable |f|
changes so that every |*| is replaced with an |* -> *|.

\begin{figure}[h]
\begin{spec}
   *        -> *     
(  * -> *)  -> * -> *
\end{spec}
\caption{Kind of |ix| for |FixA| vs. kind of |ix| for |HFixA|.}
\end{figure}

Because the kind of |f| grows, the kind of the type variable |a| grows with it.
Again, every |*| is replaced by a |* -> *|.

\begin{figure}[h]
\begin{spec}
(     *        -> *       )  ->    *        -> *
(  (  * -> *)  -> * -> *  )  -> (  * -> *)  -> * -> *
\end{spec}
\caption{Kind of |f| for |FixA| vs. kind of |ix| for |HFixA|.}
\end{figure}

This change in kinds has a lot of impact on the rest of our framework.
Introducing one additional type index forces us to changes almost all the
types in our system. We start by defining the indexed identity annotation.

> newtype HId f a ix = HId { unHId :: f a ix } 

As the type shows, the identity annotation also takes the additional type index
|ix| and uses this to parametrize the indexed functor |f|. Although this
annotation is similar to the regular identity annotation defined in
section \ref{sec:fixann}, it is clear why we cannot reuse the original type: all the kinds have changed.

Now we can create a annotation free fixed point combinator again by
parametrizing the annotated fixed point with the identity annotation.

> type HFix f ix = HFixA HId f ix

\section{Finger tree as GADT} 
\label{sec:fingertree}

To illustrate the usage of the higher order fixed point combinator we now
model a finger tree data type as a indexed GADT. The finger tree is a purely
functional data structure that can be used to model an abstract sequence with
interesting runtime behaviour. Hinze and Paterson \cite{fingertree} show
how to implement a finger tree in Haskell using a nested datatype. The
datatypes the authors describe looks more or less\footnote{In their paper Hinze
and Paterson use a slightly less constraint type in which they encode the digit
as a Haskell list datatype, with the implicit invariant this list has length
greater than or equal to 1 and less than or equal to 4. We use the |Digit|
datatype with four constructors to be a bit more explicit.} like the following:

\begin{spec}
data Node  a = Node2 a a  | Node3 a a a

data Digit a = Digit1 a | Digit2 a a | Digit3 a a a | Digit4 a a a a

data FingerTree a 
  =  Empty
  |  Single a
  |  Deep  (Digit a)
           (FingerTree (Node a))
           (Digit a)
\end{spec}

The |FingerTree| datatype can be seen as a spine structure containing zero, one
or two 2-3 trees per spine node. In this example 2-3 trees are trees that
branch with a factor of 2 or a factor of 3 in their recursive positions. To
allow a bit more flexibility at the root nodes of the 2-3 trees the |Digit|
type is used to allow a branching factor between 1 and 4.  Hinze and Paterson
show that flexing the branching factor on the top-level of the 2-3 trees makes
it easier to write algorithms and efficiency proofs for finger trees, but proof
this does not negatively influence the asymptotic running time of the
algorithms. Finger trees containing zero or one 2-3 trees must be created with
the |Empty| or |Single| constructors and have only one spine node.  Larger
finger trees containing more spine nodes are constructed using the |Deep|
constructor. The |Deep| constructor takes two 2-3 trees -- with a |Digit| root
node -- and take one extra spine node for the rest of the tree. At every
recursive position the value type changes by surrounding it with one |Node|
level. The usage of the |Node| type for the recursion implicitly encodes the
depth of the 2-3 trees at every level of the spine grows with one.

The paragraph above shows that the nested finger tree datatypes encodes some
important invariants about the structure in its type. Before we try to
encode the same structure as an indexed GADT we extract an explicit
specification from the nested datatype definition above.

After identifying these structural properties we create a specification for the
definition of finger trees. We use this specification to encode finger
trees as a GADT.

\begin{enumerate}

\item A finger tree contains \emph{spine nodes}, \emph{digit nodes}, \emph{2-3
nodes} and \emph{values}.

\item 
A spine node is either an |Empty| node, a |Single| node containing one
2-3 tree or a |Deep| node containing two 2-3 trees and a tail finger tree. The spine
nodes form a linearly linked list.

\item 
The first spine node in a non-singleton finger tree only contains two
\emph{single-level} 2-3 trees. The second spine node only contains two
\emph{two-level} 2-3 trees, the third level stores two \emph{three-level} 2-3
trees and so on.  A quick calculation shows that every spine node in a finger
tree, except for the last terminating node, contains a maximum of $2 * 4 * 3^n$
values, where |n| is the depth index of the node.

\item 
A 2-3 tree is either a single value of type |a|, a branch node with 2 or
3 sub trees or a root node which is called a digit.

\item 
Digits have a branching factor of 1, 2, 3 or 4. Digits are the root nodes
of 2-3 tree and can only contain nodes or values.

\end{enumerate} 

We can now try to capture the specification, including the invariants between
the different nodes, in an indexed GADT. Our finger tree GADT contains a
single product type as the index. The product contains two components, one for
the kind of finger tree node and one for the encoding of the three depths. We
first define three phantom types for indices to indicate a spine node, a digit
node and a 2-3 tree node.

> data Sp
> data Dg
> data Nd 

The depth encoding is written down as a subscript type level natural
number, similar to the index of the |Vector| datatype above. The depth index
forms the second component of the index tuple.

> type SpI i  = (Sp,  i)
> type DgI i  = (Dg,  i)
> type NdI i  = (Nd,  i) 

Now we can define the finger tree GADT that encodes all the invariants from the
specification.

> data Tree (b :: *) (f :: * -> *) :: * -> * where
>   Empty   ::                                                               Tree b f (SpI (Succ i))
>   Single  :: f (DgI (Succ i))                                          ->  Tree b f (SpI (Succ i))
>   Deep    :: f (DgI (Succ i)) -> f (SpI (Suc2 i)) -> f (DgI (Succ i))  ->  Tree b f (SpI (Succ i))
>
>   Digit1  :: f (NdI i)                                                 ->  Tree b f (DgI (Succ i))
>   Digit2  :: f (NdI i) -> f (NdI i)                                    ->  Tree b f (DgI (Succ i))
>   Digit3  :: f (NdI i) -> f (NdI i) -> f (NdI i)                       ->  Tree b f (DgI (Succ i))
>   Digit4  :: f (NdI i) -> f (NdI i) -> f (NdI i) -> f (NdI i)          ->  Tree b f (DgI (Succ i))
>
>   Node2   :: f (NdI i) -> f (NdI i)                                    ->  Tree b f (NdI (Succ i))
>   Node3   :: f (NdI i) -> f (NdI i) -> f (NdI i)                       ->  Tree b f (NdI (Succ i))
>   Value0  :: b                                                         ->  Tree b f (NdI Zero) 

The type parameter |b| is used for the value type that is stored in the
sequence.  The finger tree has an explicit type parameter |f| for the recursive
positions, just like the binary tree example for the regular recursive
datatypes. Both the finger tree datatype itself and the type variable for the
recursive positions have kind |* -> *| and expect a type index. Every
constructor produces a finger tree with its own specific index and can enforce
a constraint between this index and the index of the recursive positions.  

The definition shows that the |Empty|, |Single| and |Deep| constructors all
create spine nodes with a non-zero depth index. A |Single| spine node contains
one |Digit| root node of a 2-3 tree with exactly the same non-zero depth as the
spine node encodes. This constraint is enforced in the index parameter for the recursive
structure |f|. The |Deep| spine node contains two |Digit| root nodes for two 2-3
trees, also with the same depth index. The tail finger tree stored in the |Deep|
constructor's second argument must also be a spine node with a depth index
one larger than the current spine node. This enforces the tail to store more
values than the head of a finger tree as specified in point 3 of the
specification.  

There are four different constructors for creating digit nodes, one for each
arity. The digit constructors all take sub trees with the |Nd| index at the
recursive positions, which can mean both 2-3 branching nodes or plain values.
The digit nodes all have a depth index one larger than their sub trees.  

Like digit nodes, the 2-3 node constructors |Node2| and |Node3| take sub trees
with the |Nd| index at the recursive positions. This means a 2-3 branching node
again or plain values. Also, the depth index is one larger than the index of
their sub trees.  A value is special kind of node that always has index zero.
It contains a value of type |b| which is the value type parameter of the finger
tree datatype.  

\begin{figure}[hp]
\hspace{-5.0cm}\includegraphics[scale=0.45]{./fingertree.pdf}
\caption{Example finger tree. This image shows the structure of finger trees
and the positioning of spine nodes, digits, 2-3 nodes and value. The black
boxes at the bottom of each node show the type level index.}
\end{figure}

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
derived |Functor| and |Traversable| instances for our binary tree
example datatype. These traversal functions allow us to perform a restricted form
of generic programming over our datatypes. Generic traversals form an essential
part of our generic persistence framework. Unfortunately, these type classes
cannot be used for our higher order datatypes like our finger tree GADT. In
order to perform generic traversal over indexed datatypes we have to introduce
higher order variant of these three type classes.

\subsection{Functor type class} 

Ghani and Johann \cite{initial} describe how to create a type class for higher
order functors. The functorial map function works on indexed datatypes.

> class HFunctor h where
>   hfmap :: (forall ix. a ix -> b ix) -> forall ix. h a ix -> h b ix 

We define a derived higher order functor type class |PFunctor| that gets an
additional proof object |phi|. This object proves the index is an inhabitant of
a specific \emph{family}. This allows us to write functor instances that work
not for all type indices, but for an explicitly limited set.

> class PFunctor phi h where
>   pfmap  :: (  forall ix. phi ix ->    a ix ->    b ix)
>          ->    forall ix. phi ix -> h  a ix -> h  b ix

When we compare both the |hfmap| and the |pfmap| signatures with the original
|fmap| signature we clearly see the pattern:

\begin{spec}
fmap   :: (                      a     -> b     ) ->                       f  a     -> f  b
hfmap  :: (forall ix.            a ix  -> b ix  ) -> forall ix.            h  a ix  -> h  b ix 
pfmap  :: (forall ix. phi ix ->  a ix  -> b ix  ) -> forall ix. phi ix ->  h  a ix  -> h  b ix 
\end{spec}

In the next section we show how to create an finger tree instances for the
|PFunctor| type class and see how the proof object can be used to limit the set
of possible indices.

\subsection{Finger tree functor instance} 

The |PFoldable| type class forces us to make explicit the index family we want to reason about,
before we can create an instance for the finger tree datatype.
We construct a GADT that serves as a proof object that proves that a certain
index is actually a possible index for our |Tree| GADT. We indicate proof
types and proof values with a postfix $\phi$.

> data TreePhi :: * -> * where
>   SpPrf   :: NatPrf c ->  TreePhi (SpI (Succ c))
>   DgPrf   :: NatPrf c ->  TreePhi (DgI (Succ c))
>   NdPrf   :: NatPrf c ->  TreePhi (NdI (Succ c))
>   NdZPrf  ::              TreePhi (NdI Zero) 

This proof type can be used to show that for every natural number there is a
\emph{spine}, \emph{digit} and \emph{node} index for the successor of that
natural number.  Note that the structure of the |TreePhi| proof indices is
similar to the structure of the finger tree indices.  For this proof object we
also need a natural number proof.

> data NatPrf :: * -> * where
>   ZeroP  ::              NatPrf Zero
>   SuccP  :: NatPrf n ->  NatPrf (Succ n) 

This datatype can be used to construct a value for every natural number index.
Again, the structure of the proof type follows the structure of the natural
numbers.  

No we can create a |PFunctor| instance in which we
can pattern match on both the proof object and the finger tree constructor.
Because the type signature of |pfmap| parametrizes the proof |TreePhi| and the
constructor |Tree b| with the same index, the compiler is able to see
that there are only a limited set of proof/constructor combinations possible.  

The |PFunctor| instance for finger trees becomes:

\begin{small}

> instance PFunctor TreePhi (Tree b) where
>   pfmap f phi h =
>     case (phi, h) of
>       (SpPrf  _,          Empty)           -> Empty
>       (SpPrf  p,          Single a)        -> Single  (f (DgPrf p) a)
>       (SpPrf  p,          Deep a c b)      -> Deep    (f (DgPrf p) a)
>                                                       (f (SpPrf (SuccP p)) c)
>                                                       (f (DgPrf p) b)
>       (DgPrf  (SuccP p),  Digit1 a)        -> Digit1  (f (NdPrf p) a)
>       (DgPrf  (SuccP p),  Digit2 a b)      -> Digit2  (f (NdPrf p) a)
>                                                       (f (NdPrf p) b)
>       (DgPrf  (SuccP p),  Digit3 a b c)    -> Digit3  (f (NdPrf p) a)
>                                                       (f (NdPrf p) b)
>                                                       (f (NdPrf p) c)
>       (DgPrf  (SuccP p),  Digit4 a b c d)  -> Digit4  (f (NdPrf p) a) 
>                                                       (f (NdPrf p) b)
>                                                       (f (NdPrf p) c) 
>                                                       (f (NdPrf p) d)
>       (NdPrf  (SuccP p),  Node2 a b)       -> Node2   (f (NdPrf p) a) 
>                                                       (f (NdPrf p) b)
>       (NdPrf  (SuccP p),  Node3 a b c)     -> Node3   (f (NdPrf p) a) 
>                                                       (f (NdPrf p) b)
>                                                       (f (NdPrf p) c)
>       (DgPrf  ZeroP,      Digit1 a)        -> Digit1  (f NdZPrf a)
>       (DgPrf  ZeroP,      Digit2 a b)      -> Digit2  (f NdZPrf a) 
>                                                       (f NdZPrf b)
>       (DgPrf  ZeroP,      Digit3 a b c)    -> Digit3  (f NdZPrf a) 
>                                                       (f NdZPrf b) 
>                                                       (f NdZPrf c)
>       (DgPrf  ZeroP,      Digit4 a b c d)  -> Digit4  (f NdZPrf a) 
>                                                       (f NdZPrf b)
>                                                       (f NdZPrf c) 
>                                                       (f NdZPrf d)
>       (NdZPrf,            Value0 a)        -> Value0  a

\end{small} 

To allow the |pfmap| function for this instance to apply the map function |f|
to all the sub trees, we need to construct an appropriate proof again. We can do
this by unpacking the input proof, repacking this with the right constructor
and passsing it to the function |f|. This works out exactly right because the proof
type follows the same index structure as our finger tree.  

This |PFunctor| instance allows us to map a function over one level of
recursive positions of the finger tree GADT. We can use the proof object to
distinguish between different positions in the structure. The |PFunctor|
instances forms the basis of generic traversals over higher order
datatypes with restricted families of indices.

\subsection{Traversable type class} 

Besides the higher order \emph{functor} instance we can also make a higher order
\emph{traversable} instance, allowing us to perform effectful traversals. First we
define a |PTraversable| type class similar to the |PFunctor| type class, we also
use a proof object $\phi$ to restrict the family of indices.

> class PFunctor phi h => PTraversable phi h where
>   ptraverse  ::  Applicative f
>              =>  (  forall ix. phi ix ->    a ix -> f (   b ix))
>              ->     forall ix. phi ix -> h  a ix -> f (h  b ix) 

So if we are provided an effecful computation for the element type |a ix| in
some |Applicative| -- or possibly monadic -- context |f|, the |ptraverse|
function is able to apply this to all elements of the structure
|h a ix|.
 
\subsection{Finger tree traversable instance} 

The |PTraversable| instance for the finger tree GADT follows the
same pattern as the regular |Traversable| instance. But this indexed variant
requires us to pattern match on- and recursively supply the proof objects. This
is similar to the |PFunctor| instance.

\begin{small}

> instance PTraversable TreePhi (Tree a) where
>   ptraverse f phi h =
>     case (phi, h) of
>       (SpPrf _,          Empty)           -> pure Empty
>       (SpPrf p,          Single a)        -> pure Single  <*>  (f (DgPrf p) a)
>       (SpPrf p,          Deep   a c b)    -> pure Deep    <*>  (f (DgPrf p) a)
>                                                           <*>  (f (SpPrf (SuccP p)) c) 
>                                                           <*>  (f (DgPrf p) b)
>       (DgPrf (SuccP p),  Digit1 a)        -> pure Digit1  <*>  (f (NdPrf p) a)
>       (DgPrf (SuccP p),  Digit2 a b)      -> pure Digit2  <*>  (f (NdPrf p) a)
>                                                           <*>  (f (NdPrf p) b)
>       (DgPrf (SuccP p),  Digit3 a b c)    -> pure Digit3  <*>  (f (NdPrf p) a)
>                                                           <*>  (f (NdPrf p) b)
>                                                           <*>  (f (NdPrf p) c)
>       (DgPrf (SuccP p),  Digit4 a b c d)  -> pure Digit4  <*>  (f (NdPrf p) a)
>                                                           <*>  (f (NdPrf p) b)
>                                                           <*>  (f (NdPrf p) c)
>                                                           <*>  (f (NdPrf p) d)
>       (NdPrf (SuccP p),  Node2  a b)      -> pure Node2   <*>  (f (NdPrf p) a)
>                                                           <*>  (f (NdPrf p) b)
>       (NdPrf (SuccP p),  Node3  a b c)    -> pure Node3   <*>  (f (NdPrf p) a)
>                                                           <*>  (f (NdPrf p) b)
>                                                           <*>  (f (NdPrf p) c)
>       (DgPrf ZeroP,      Digit1 a)        -> pure Digit1  <*>  (f NdZPrf a)
>       (DgPrf ZeroP,      Digit2 a b)      -> pure Digit2  <*>  (f NdZPrf a)
>                                                           <*>  (f NdZPrf b)
>       (DgPrf ZeroP,      Digit3 a b c)    -> pure Digit3  <*>  (f NdZPrf a)
>                                                           <*>  (f NdZPrf b)
>                                                           <*>  (f NdZPrf c)
>       (DgPrf ZeroP,      Digit4 a b c d)  -> pure Digit4  <*>  (f NdZPrf a)
>                                                           <*>  (f NdZPrf b)
>                                                           <*>  (f NdZPrf c)
>                                                           <*>  (f NdZPrf d)  
>       (NdZPrf,           Value0 a)        -> pure (Value0 a) 

\end{small} 

both the higher order |PFunctor| and |PTraversable| instances for our finger
tree GADT, we can now start writing generic recursive traversals.

\section{Higher order annotation classes} 

In our generic annotation framework for regular datatypes we have created
three type classes to associate custom functionality with wrapping and
unwrapping annotations. We have to do the same for our higher order annotation
framework, but we cannot use the existing type classes due a clear type
mismatch. In this chapter we extend the |AnnI| and |AnnO| type classes to
work with indexed data types and show how make instances for the higher order
identity annotation.

First, recall the type signatures of the original annotation functions.

\begin{spec}
type In     a f m  =    f (  FixA a f)  ->  m       (FixA a f   )
type Out    a f m  =         FixA a f   ->  m (  f  (FixA a f)  )
type InOut  a f m  = (  f (  FixA a f)  ->       f  (FixA a f)  )
                   ->        FixA a f   ->  m       (FixA a f   ) 
\end{spec}

We now define the three type signatures for the higher order query, producer
and modifier functions. The type signatures already make clear the difference
between annotation working on indexed types and our previous annotations
working on regular recursive data structures.

> type HIn     a h phi m  =    forall ix. phi ix ->  h (  HFixA a h)  ix ->  m       (HFixA a h   ix)
> type HOut    a h phi m  =    forall ix. phi ix ->       HFixA a h   ix ->  m (  h  (HFixA a h)  ix)
> type HInOut  a h phi m  = (  forall ix. phi ix ->  h (  HFixA a h)  ix ->       h  (HFixA a h)  ix)
>                         ->   forall ix. phi ix ->       HFixA a h   ix ->  m       (HFixA a h   ix) 

All three function types take an annotation |a|, a higher order recursive
structure |h| with explicit recursive positions, an index family proof object
$\phi$ and some monadic context |m|.  

The |HIn| type describes a producer function that takes a single unannotated
node with fully recursive sub structures and wraps it with some annotation,
possibly inside some effectful context. The functions can restrict the family
of indices with the proof term $\phi$. The |HOut| type describes a query
function that performs the dual task of the producer. It takes a fully
annotated tree and unwraps the root annotation, returning a single unannotated
node with fully annotated sub structures. The modifier function |HInOut| simply
combines the query and producer functions in one step.  

Now we introduce the three type classes that implement respectively a producer,
a query and a modifier function. 

> class (Applicative m, Monad m) => HAnnO a h phi m where
>   hannO :: HOut a h phi m
> 
> class (Applicative m, Monad m) => HAnnI a h phi m where
>   hannI :: HIn a h phi m 

Again, we create a modifier type class that has a default implementation in
terms of the query and producer function.

> class (HAnnO a h phi m, HAnnI a h phi m) => HAnnIO a h phi m where
>   hannIO :: HInOut a h phi m
>   hannIO f phi = hannI phi . f phi <=< hannO phi 

The instances for the |HId| annotation are as simple as wrapping and unwrapping
the constructor.

> instance (Applicative m, Monad m) => HAnnO HId h phi m where
>   hannO _ (HInA (HId f))  = return f
>   hannO _ (HInF      f )  = return f
> 
> instance (Applicative m, Monad m) => HAnnI HId h phi m where
>   hannI _ = return . HInA . HId
> 
> instance (Applicative m, Monad m) => HAnnIO HId h phi m 

And from these type classes we can simply derive the two functions to fullly
annotate or fully strip all annotations from the top of a tree.

> hfullyIn  ::  (HAnnI a h phi m, PTraversable phi h)
>           =>  phi ix -> HFixA a h ix -> m (HFixA a h ix)
>
> hfullyIn phi (HInF f)  = ptraverse hfullyIn phi f >>= hannI phi
> hfullyIn _   a         = return a

> hfullyOut  ::  (HAnnO a h phi m, PTraversable phi h)
>            =>  phi ix -> HFixA a h ix -> m (HFixA a h ix)
>
> hfullyOut phi (HInA f)  = hannO phi (HInA f) >>= fmap HInF . ptraverse hfullyOut phi
> hfullyOut _   a         = return a 

Although the types have changed, the annotation framework is similar to the
one for regular recursive data types. We can now use these type classes to
implement a paramorphism for indexed datatypes

\section{Higher order paramorphism}

In this section we introduce paramorphic traversals for higher order datatypes.
In order to express the algebras we define the lifted sum and product types,
similar to the Haskell |Either a b| and |(a, b)| (tuple) types, but with an
additional type index.

%if False

> infixl 6 :+:
> infixl 7 :*:

%endif

> data (f :+:  g) ix =  L (f ix) | R (g ix)
> data (f :*:  g) ix =  (:*:) { hfst :: f ix, hsnd :: g ix }

Using the product type we can construct a higher order paramorphic algebra.
Like the algebra for regular datatypes, this algebra is able to
destruct one node with the recursive results in the recursive positions to a
some result value. This indexed algebra can use a proof object as a restriction on the
index family. Both the input structure |f| and the output structure |g| are
indexed with the same index type |ix|. Because this is a paramorphic algebra,
the |f| structure contains both the recursive results and the fully annotated
sub structures.

> type HPsiA a phi f g = forall ix. phi ix -> f (HFixA a f :*: g) ix -> g ix

Not that this algebra contains the type variable $\alpha$ and can be used for
annotated traversals.

Now we define the higher order annotated paramorphism that uses an algebra to
recursively destruct the structure |HFixA a f ix| into a value of type |g ix|.
Because this is an annotated paramorphism the traversal uses the |hannO|
method from the |HAnnO| type class to unwrap the annotation, possibly in an
effecful context |m|.

> hparaMA
>   :: (HAnnO a f phi m, PTraversable phi f)
>   => HPsiA a phi f g -> phi ix -> HFixA a f ix -> m (g ix)
> hparaMA psi phi = return . psi phi <=<
>   ptraverse (\p x -> (| (x :*:) (hparaMA psi p x) |)) phi <=< hannO phi

\section{Sum, product, concat, and contains algebras}

To illustrate the usage of the higher order paramorphism we define four
example algebras for our finger tree datatype. All four algebras are defined in
terms of one generic algebra that converts all value in a finger tree into some
monoid value and appends these together using the |`mappend`| operator. The
functionality of this algebra is similar to the |foldMap| function of the
|Foldable| type class.

The monoid type we use as the result type is a plain type without any
indices. Because the algebra forces the result type to have an index as well,
we have to explicitly ignore the index when dealing with simple Haskell types.
We do this by introducing the constant functor |K1|, that accepts a value type
and an index type, but only uses the value type and ignores the index type.

> newtype K1 h a = K { unK :: h }

We can now create a generic fold algebra that returns some monoid |m| in the
constant functor.

> foldmAlg :: Monoid m => (b -> m) -> HPsiA a phi (Tree b) (K1 m)
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
>     Value0 a         -> K (f a)
>  where g = unK . hsnd

The first parameter of this algebra is a function that converts the values
stored in the finger tree into some type |m| for which there is a |Monoid|
instance available. Because this algebra is a paramorphic algebra the function
can use both the recursive results and the original sub structures. Note that the
|foldmAlg| only uses the recursive results, which actually makes it a
\emph{catamorphism}.

The paramorphism also allows us to add an index type to the result value,
because we do not use this we ignore the index using the |K| constructor.
Unpacking the recursive result from the tuple and from the |K| type is done
with the helper function |g|.

The |Monoid| type class allows us to be generic in the type we want to fold. By
specializing the value type to |Int| and the output type to |Sum Int| or
|Product Int| \footnote{The |Sum| and |Product| newtypes can be found in
Haskell's |Data.Moinoid| package. They are used to specialize the way the
|`mappend`| operators combines two numeric values, either with addition or with
multiplication.}, we can create two algebras that respectively compute the sum
and the product of a sequence containing integers.

> sumAlg :: HPsiA a phi (Tree Int) (K1 (Sum Int))
> sumAlg = foldmAlg Sum

> productAlg :: HPsiA a phi (Tree Int) (K1 (Product Int))
> productAlg = foldmAlg Product

Creating a fold algebra that concatenates all strings can simply be done by
exploiting the default monoid instances for lists and parametrize |foldmAlg|
with the identity function.

> concatAlg :: HPsiA a phi (Tree String) (K1 (String))
> concatAlg = foldmAlg id

The last example is a containment check. This algebra uses an equality check in
combination with the |Any| monoid wrapper to check whether a certain value
exists in the finger tree sequence.

> containsAlg :: Eq b => b -> HPsiA a phi (Tree b) (K1 Any)
> containsAlg v = foldmAlg (Any . (==v))

These four algebras can now be lifted to true annotated traversals using the
|hparaMA| function from the previous section. Note that in all four cases we
supply an index proof to the paramorphism. This proof object contains the same index
as the root of our finger tree, which is |SpPrf ZeroP|. After computing the
result we unpack it from the constant functor |K| and from the monoid wrapper
when needed. The derived functions are shown below.

> sum  ::  HAnnO a (Tree Int) TreePhi m
>      =>  FingerTreeA a Int -> m Int
>
> sum h = (| (getSum . unK) (hparaMA sumAlg (SpPrf ZeroP) h) |)

> product  ::  HAnnO a (Tree Int) TreePhi m
>          =>  FingerTreeA a Int -> m Int
>
> product h = (| (getProduct . unK) (hparaMA productAlg (SpPrf ZeroP) h) |)

> concat  ::  HAnnO a (Tree String) TreePhi m
>         =>  FingerTreeA a String -> m String
>
> concat h = (| unK (hparaMA concatAlg (SpPrf ZeroP) h) |)

> contains  ::  (Eq b, HAnnO a (Tree b) TreePhi m)
>           =>  b -> FingerTreeA a b -> m Bool
>
> contains v h = (| (getAny . unK) (hparaMA (containsAlg v) (SpPrf ZeroP) h) |)

These four algebras show that it does not take that much to implement simple
catamorphisms, that compute values of simple Haskell types, over indexed data
structures. In the next section we show a more complex example, the |cons|
function that appends one item to the beginning of the finger tree sequence.

\section{Prepend algebra}

Two of the basic operations on finger trees as described in the paper by Hinze
and Paterson are the |cons| and the |snoc| functions. These functions prepend or
append one item to the sequence. Two seemingly simple functions. However, both
functions are a bit involved because they have to preserve the nested structure
of the finger tree. In this section we briefly show what is needed to write a
paramorphic algebra for the |cons| function on our finger tree GADT.

It takes some work to work out the type correct type signature for the algebra
of the |cons| function.  We need to construct a paramorphism that takes as
input the original annotated finger tree and computes a function that takes the
node to prepend and results in the new finger tree. We do need a lot of type
level helpers to ensure the paramorphism resulting in an prepend function
conforms all the type level invariants encoded in the GADT.

First we define two type families to compute the first and the second type
level component from our indexed product type. Both type level families simply
project the left or right component and apply the original index to the result.

> type family Fst a :: *
> type instance Fst ((a :*: b) ix) = a ix
  
> type family Snd a :: *
> type instance Snd ((a :*: b) ix) = b ix

The prepend function that is computed by the paramorphic |cons| algebra needs
itself to reason about the index types. We need a higher order function type
that distributes an index to both the co- and contra variant positions.

> infixr 1 :->
> data (:->) a b ix = F { unF :: a ix -> b ix }

We define the |#| operator to apply an indexed function to an indexed argument.

> (#) :: (a :-> b) ix -> a ix -> b ix
> (#) (F x) y = x y

The inductive structure of the GADT forces us to reason about the numeric index
relation of two connected nodes. Computing the successor of a type level
natural number can be done using the successor (written down as |Succ n|)
datatype. We now introduce a predecessor type family (written down as |Pred n|)
that decrements any non-zero type level natural with one.

> type family Pred a
> type instance Pred (Succ c) = c

Now we introduce two type level functions, encoded as datatypes, that allow
us to change the GADT index. The |N| datatype takes a container datatype and an
index and returns the same container type but with a different index.
The |N| type level function forces the result to have a node type by using the
|Nd| phantom type as the first component of index tuple. A |Maybe| type is to
indicate a possible empty result. The |N| data function keeps the depth index
intact. 

> newtype N  f ix = N (Maybe (f (Nd, Snd ix)))

The |D1| type level function encodes a similar pattern, but decrements
the depth index by one using the predecessor type family defined above.

> newtype D1  f ix = D1 (f (Fst ix, Pred (Snd ix)))

We now have all the components to write down the type signature for the
paramorphic |cons| algebra.  The type is rather complicated because it encodes
all invariants involved when appending an item to the head of the sequence.

> consAlg  ::  tree ~ HFixA a (Tree b)
>          =>  HPsiA a TreePhi (Tree b)
>                (N (D1 tree) :-> tree :*: N tree)

%if False

> consAlg (SpPrf _) = undefined

%endif

Let try to explain what this type means. 

To simplify the type signature a type variable |tree| is defined as a shortcut
for a fully annotated finger tree structure with value of type |b|. A type
equality is used to make this local definition.  We have a paramorphic algebra
that takes an annotated finger tree as input and computes a function from a
finger tree \emph{node} to a product of some finger tree and some finger tree
\emph{node}.  The result is a product type because it returns both the new
finger tree and possibly a overflow node that needs to be inserted one level
deeper. So, if we push a node to the head of sequence it gets inserted in the
left 2-3 tree of the first spine node. When this 2-3 tree is already full one
node (sub-tree) is selected to be apppended to a spine node one deeper.  The
input node always has a depth index one less than depth index of the sub-tree
being traversed by the algebra, indicated by the |D1| type.  The result tree
index is unaffected, this makes sense: appending a node to a finger tree
returns a finger tree with the same index.

Both the input and overflow part of the output of the computed functions are
nodes, because only values can be inserted inserted into a finger tree and only
2-3 trees can overflow to a deeper level. The |N| type also encodes the
optionality using the |Maybe| type, this allows the algebra to stop the
insertion when no node overflows. Hinze and Paterson \cite{fingertree} prove
that overflowing happens rarely and the amortized time for appending a value is
still $O(1)$.

One of the constraints of the |consAlg| is that we can only insert nodes with a
depth index one less than the tree it gets appended to. At the top level a
finger tree is always a spine node with depth index one, as can be seen in the
|FingerTreeA| type synonym. This means we can only insert nodes with depth
index zero into a top level finger tree. This is exactly as we would expect:
only |Value0| nodes have index zero. The |D1| type family can only be applied to
non-zero indices. To proof to the compiler our input finger tree always has a
non-zero index we parametrize the algebra with our |TreePhi| proof object.

\subsection{Prepend function}
\label{sec:prepend}

Now that we have shown the type signature of the |consAlg| function, we can
lift it using the |hparaMA| function to a true prepend function. Hinze and
Paterson name this left-biased prepend function with the left pointing
triangle: $\triangleleft$

The prepend function takes a value |x| and appends this to the existing
sequence |xs|. The output of running the algebra against the |xs| is not a
simple value but a \emph{function}. This function takes the original finger
tree to the result finger tree.  We apply this function to the value |x| to get
back the result finger tree.  But, because we are working with annotations, we
have to manually annotate the new parts of this result sequence with the
|hfullyIn| function.

> (<|)  ::  HAnnIO a (Tree b) TreePhi m
>       =>  b -> FingerTreeA a b -> m (FingerTreeA a b)
>
> x <| xs = do  fun <- hparaMA consAlg (SpPrf ZeroP) xs
>               let res = hfst (fun # (N . Just . D1) (HInF (Value0 x)))
>               hfullyIn (SpPrf ZeroP) res

As can be seen in the definition of the function, we have to include a lot of
boilerplate code to pack and unpack all the intermediate wrappers. These
wrappers, mostly type level functions, were introduced to allow working with
indexed datatypes.

This example shows it is still possible to abstract away from recursion when
writing more complicated operations over indexed datatypes. Higher order
algebras can be written that preserve all invariants encoded in a GATD.
Lots of helper type level wrappers and functions have to be defined, only to be able
to express the types. These wrappers force our implementation to include more
boilerplate code which makes it harder to write.

\section{Persistent Annotation}

%if False

> hread = undefined
> hwrite = undefined
> instance Binary (Pointer (f b ix)) where { put = undefined; get = undefined }
> instance (HBinary TreePhi (HP (Tree Int) (HFixA HP (Tree Int))))

%endif

In order to serialize indexed datatypes to disk we need to make the |Pointer|
type \ref{sec:heaplayout} an instance of our higher order annotation type
classes. All the types and type classes involved need to be lifted to the
indexed level, meaning we can not reuse any code of our previous framework.

For example, we cannot use the regular |Binary| type class, because this class
only works for non-indexed types.  So we create a higher order |HBinary| class
useful for serializing and deserializing index datatypes.

> class HBinary phi h where
>   hput :: phi ix -> h ix -> Put
>   hget :: phi ix -> Get (h ix)

Now we need to create an |HBinary| instance for our finger tree. We only show
the class header and skip the implementation, which is rather straightforward.
Note that for the element type |b| we still require a regular |Binary|
  instance.

> instance (Binary b, HBinary TreePhi f) => HBinary TreePhi (Tree b f)

The second problem we encounter is that we cannot reuse the |read| and |write|
functions working on our storage heap.  Also these functions have to be lifted
to an |hread| and an |hwrite| accordingly.

> hread   :: HBinary phi h => phi ix -> Pointer (h ix) -> HeapR (h ix)
> hwrite  :: HBinary phi h => phi ix -> h ix -> HeapW (Pointer (h ix))

Now we lift the pointer annotation to a higher order level by extending the
kinds. Because Haskell has no \emph{kind polymorphism} and we need the
additional type parameter |ix| we cannot reuse the existing |P| type.

> newtype  HP  (f  :: (* -> *) -> * -> *)
>              (b  :: * -> *)
>              (ix :: *)
>       =  HP  { unP :: Pointer (f b ix) }

And now we can give the |HAnnO|, |HAnnI| and |HAnnIO| instances for the
|HP| annotation in both the read and read-write heap contexts. The
implementations are rather straightforward and are shown below.

> instance HBinary phi (h (HFixA HP h)) => HAnnO HP h phi HeapR where
>   hannO phi  (HInA (HP  h)  ) = hread phi h
>   hannO _    (HInF      h   ) = return h

> instance HBinary phi (h (HFixA HP h)) => HAnnO HP h phi HeapW where
>   hannO phi  (HInA (HP h)  ) = liftR (hread phi h)
>   hannO _    (HInF     h   ) = return h

> instance HBinary phi (h (HFixA HP h)) => HAnnI HP h phi HeapW where
>   hannI phi = fmap (HInA . HP) . hwrite phi

> instance HBinary phi (h (HFixA HP h)) => HAnnIO HP h phi HeapW where

These instances require that the higher order fixed point combinator must also
be an instance of the |HBinary| class.

> instance HBinary phi (a f (HFixA a f)) => HBinary phi (HFixA a f) where
>   hput phi  (HInA f)  = hput phi f
>   hget phi            = (| HInA (hget phi) |)

Again, this section makes clear what the implication are of moving away from
regular datatypes to indexed datatypes. From the high level annotation
instances to the lower level read and write actions to the type class for
binary serialization, everything has to be lifted to the indexed level.

\section{Putting it all together}

In the previous section we have in parallel, both extended the persistence
framework to index datatypes and developed an indexed finger tree type as an
example. Now we have all the ingredients to show that this higher order
framework actually works. In this concluding section we conclude the chapter by showing how to
build up a finger tree of integers on disk and compute the sum of
all values.

First we introduce a type synonym for our finger tree of integers. We define an
|IntStore| to be a persistent finger tree of integers by using the |HP|
annotation at the recursive positions.

> type IntStore = HFixA HP (Tree Int) (SpI One)

The |pempty| function is used to produce an empty persistent finger tree.

> pempty :: HeapW IntStore
> pempty = hannI (SpPrf ZeroP) Empty

By only specializing the type we lift the prepend function ($\triangleleft$)
from section \ref{sec:prepend} to work on persistent finger trees. We see that the
type signatures of our generic functions become simpler the more we specialize
them.

> pcons :: Int -> IntStore -> HeapW IntStore
> pcons = (<|)

We now have all the ingredient to write a function that creates and stores a
finger tree on disk. We use the |run| function from our heap context to open a
database file and run a heap computation.
  
\begin{spec}
createStore :: FilePath -> IO ()
createStore file = run file $
  do  p <- pcons 6 =<< pcons 3 =<< pcons 4 =<< pempty
      storeRoot p
\end{spec}

Both the |pcons| and the |pempty| function result in an persistent finger
tree, which means a |Pointer| to a finger tree. Because the |pcons| function
also takes a persistent finger tree as input we can compose these actions
together. Because the actions are monadic we composes these with the
right-to-left monadic bind operator (|=<<|). After producing the tree we store
the pointer in the root node of the heap.

We create a second function that reads a finger tree from disk and
computes the total sum over all values. First we lift the |sum| function to
work on persistent finger trees, also by only changing the type.

> psum :: IntStore -> HeapR Int
> psum = sum

Now we can write a function that reads our previously stored finger tree from
the root node and computes the sum using our lifted |psum| function.

\begin{spec}
computeSum :: FilePath -> IO ()
computeSum file = run file $
  do  o <- getRoot
      s <- psum o
      liftIO (print s)
\end{spec}

These two functions both work on the persistent store and can now be run in two
different sessions. A consecutive run of first the |createStore| and then the
|computeSum| results in the expected total 13:

\begin{verbatim}
ghci> createStore "test.db"
ghci> computeSum "test.db"
13
\end{verbatim}

This example is the proof we have been able to extend our persistent framework
to work with indexed datatypes. By lifting all the types to an indexed level we
were able to build up persistent higher order structures. When using our
annotation framework we can use the indices to reason about the position in a
higher order structure. The results of this chapter allow us to build type safe
container datatypes that encode structural invariants in their types. Although
the final result is pleasing, we must admit that writing operations on
indexed datatypes in Haskell can be a tedious job. 

