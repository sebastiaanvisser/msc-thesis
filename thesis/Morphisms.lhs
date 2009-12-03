%include polycode.fmt
%include thesis.fmt

%if False

> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE UndecidableInstances #-}
> module Morphisms where

> import Control.Applicative
> import Control.Category
> import Control.Monad.Identity
> import Control.Monad.Reader hiding (mapM)
> import System.IO.Unsafe
> import Data.Foldable
> import Data.Traversable
> import Prelude hiding ((.), id, mapM, lookup)
> import Fixpoints
> import Generics.Regular.Seq

%endif

\begin{section}{Generic traversals with annotations}

\begin{subsection}{Paramorphisms}

\review{
Now that we have a way to associate functionality with annotations we should be
able to write operations over our annotated structures and perform actions each
time we would normally go directly into recursion.  To generalize this pattern
we abstract away from recursion when writing the structure-processing
algorithms and use \emph{morphisms}. We start out with the \emph{paramorphism},
which is a generalization of the \emph{catamorphism}, a bottom up traversal
that can fold an entire structure into a single value. \docite{paramorphisms}
}

\review{
We first write down the type signature of the algebra for paramorphisms, we
call this algebra |Psi1|. 
}

> type Psi1 a f r = f (FixA a f :*: r) -> r

\noindent
\review{
This type signature describes an algebra that should be able to produce an
value of type |r| from one single node containing both the fully annotated
sub-structures \emph{and} the recursive results of the paramorphic computation.
}

\review{
An example of such an algebra is the function |containsAlg| for binary
trees.\footnote{Note that because the |containsAlg| algebra only uses the
recursive sub-results and not the original sub-structures this algebra is
actually a catamorphism, a special case of the more general paramorphism.
Because all catamorphisms are paramorphisms this does not invalidate the
example.} This algebra describes a recursive traversal over a binary tree that
checks whether a certain integer value is included in the tree or not.
}

> containsAlg :: Int -> Psi1 a Tree_f Bool
> containsAlg _  Leaf                      = False
> containsAlg v  (Branch c (_, l) (_, r))  = 
>   case v `compare` c of
>     LT  -> l
>     EQ  -> True
>     GT  -> r

\noindent
\review{
The paramorphism function performs a bottom up traversal over some
|Traversable| |Functor| and for every node applies the algebra, the result of
the algebra will be returned. The most generic version of this paramorphism
within our framework is the |paraMA1|.  This function runs is some monadic
context |m| and performs a traversal over some annotated structure |FixA a f|
using the |AnnQ| type class to perform annotation specific queries.
}

> paraMA1 :: AnnQ a f m => Psi1 a f r -> FixA a f -> m r
> paraMA1 psi = return . psi <=< mapM (group (paraMA1 psi)) <=< query
>   where group f c = fmap ((,) c) (f c)

\review{
From now on the $(_{\alpha}^m)$ postfix will be used to indicate that a
function requires a context and works on annotated strucutres.
}

\review{
The implementation of this generic paramorphism might seem a bit cryptic at
first sight, this is due to its very generic behaviour. Quickly summarized this
function performs a bottom-up traversal over a recursive structure like our
binary tree example.  As input it receives a fully annotated structure and it
uses the |query| function to get a true node out of the annotation.  The
|Traversable| instance, which is an implicit super class of the |AnnQ| class,
allows us to use the |mapM| function to recursively apply the |paraMA1|
function to the sub-structures. This recursive invocation is used to come up
with the sub-results.  The sub-results will be grouped together with the
original sub-structures these results are computed from. The original input
node with these grouped results as the values will be passed into the algebra
|psi|.  The algebra can now compute the result value for one level of the
recursive computation, possible using the results of deeper traversals.
}

\review{
To illustrate the usage of the |paraMA1| function we apply it to the
|containsAlg| algebra and get back a true function that performs a containment
check over a fully annotation binary tree.
}

> containsMA :: AnnQ a Tree_f m => Int -> FixA a Tree_f -> m Bool
> containsMA v = paraMA1 (containsAlg v)

\noindent
\review{
We can easily test this function in the interactive environment of the GHC
compiler.  We first manually construct a binary tree and constrain this to the
|IO| context and |Debug| annotation. While the binary tree is being
constructed, using our previously defined smart constructors, the debug
annotation prints out a trace of all nodes being produced.
}

\begin{verbatim}
ghci> join (branchA 3 <$> leafA <*> leafA) :: IO (TreeA Debug)
produce: Leaf
produce: Leaf
produce: Branch 3 <D Leaf> <D Leaf>
<D Branch 3 <D Leaf> <D Leaf>>
\end{verbatim}

\noindent
\review{
Now we can apply the |containsMA| function to the resulting binary tree and
check for the existence of a |Branch| with value |3|. While running this
function the debug annotation prints out a trace of all sub-structures being
queried.
}

\begin{verbatim}
ghci> containsMA 3 it
query: Branch 3 <D Leaf> <D Leaf>
query: Leaf
query: Leaf
True
\end{verbatim}

\noindent
\review{
Note that the paramorphic traversal is as strict as the context it runs in.
This means that because the |Debug| annotation requires the |IO| monad the
|containsMA| function becomes more strict then necessary. In section
\todo{section} we will describe a method to regain laziness for paramorphisms
running in strict contexts.
}

\review{
The paramorphism we have defined above is generic in the sense that it works
on structures with arbitrary annotations that run in an arbitrary context.
When an annotation does not have any requirements about the type of context to
run in we can use the |Identity| monad to create a pure paramorphic traversal.
}

> paraA1 :: (AnnQ a f Identity, Traversable f) => Psi1 a f c -> FixA a f -> c
> paraA1 psi = runIdentity . paraMA1 psi

\noindent
\review{
When we further restrict the annotation to be the identity annotation we get
back a pure paramorphism that works on plain unannotated structures.
}

> para1 :: Traversable f => Psi1 Id f c -> Fix f -> c
> para1 psi = paraA1 psi

\noindent
\review{
To illustrate this pure paramorphism we apply it to the |containsAlg| algebra
and get back a pure |contains| function.
}

> contains :: Int -> Tree -> Bool
> contains v = para1 (containsAlg v)

\end{subsection}

\begin{subsection}{Apomorphisms}

\review{
Dual to the paramorphism is the \emph{apomorphism}. Where the paramorphism
abstract away from recursion, the apomorphisms abstracts away from corecursion.
\docite{apomorphisms} Similarly, where paramorphisms use algebras to describe
recursive operations, apomorphisms use coalgebras to describe corecursive
operations. Apomorphisms are generalizations of the more well known
\emph{anamorphisms}. The standard Haskell function |unfold|, which can be used
to create lists from a seed value, is an example of an anamorphisms.
}

\review{
The coalgebra for an apomorphism, called |Phi|, takes a seed value of some type
|s| and should be able to produce a node containing either a new seed or a
new recursive structure.
}

> type Phi a f s = s -> f (s :+: FixA a f)

\noindent
\review{
From the type signature of the |Phi| coalgebra it is obvious that it is dual to
the |Psi| algebra for paramorphisms. Paramorphisms destruct recursive
structures to some result value |r|, apomorphisms construct recursive
structures from some seed value |s|.
}

To illustrate the usage of coalgebras we define the function |fromListCoalg|
that describes how to create a balanced binary tree from an input list of
integer values. \footnote{Note that because the |fromListCoalg| coalgebra only
produces new seeds, using the |Left| constructor, instead of directly producing
sub-structures it is actually an anamorphism. Because all anamorphisms are
apomorphisms this does not invalidate the example.} 

> fromListCoalg :: Phi a Tree_f [Int]
> fromListCoalg []      = Leaf
> fromListCoalg (y:ys)  =
>   let  l  = take (length ys `div` 2) ys
>        r  = drop (length l) ys
>   in Branch y (Left l) (Left r)

\noindent
Like the paramorphism we start with an apomorphism that corecursively generates
an annotated structure in some, possibly monadic, context. We call this
function |apoMA|. This apomorphism takes a coalgebra |Phi| and some initial
seed value |s| and uses this to produce an annotated structure |FixA a f|.

> apoMA :: AnnP a f m => Phi a f s -> s -> m (FixA a f)
> apoMA phi = produce <=< mapM (apoMA phi `either` return) . phi

\noindent
This apomorphism first applies the algebra |phi| to the initial seed value |s|
and new structure |f| with either a new seed or an recursive sub-structure in
the sub-positions. When a new seed has been supplied by the coalgebra the
|apoMA| function will be call recursively to produce a new sub-structure. When
the coalgebra supplied an existing sub-structure the |produce| function from
the |AnnP| type class will be used to provide an annotation for it. The entire
structure itself will be supplied with an annotation as well using the
|produce| function again.

Now we can apply this to our example coalgebra |fromListCoalg| and get back a
true fromList function that can be used to produce annotation binary trees.

> fromListMA :: AnnP a Tree_f m => [Int] -> m (FixA a Tree_f)
> fromListMA = apoMA fromListCoalg

Now we can illustrate the usage of the |fromListMA| function by construction a
simple binary tree from a two-element lists. Again we constraint the context to
|IO| and the annotation to |Debug|. The annotation nicely prints out all the
sub-structures that are being produced before the final result tree is
returned.

\begin{verbatim}
ghci> fromListMA [1, 3] :: IO (FixA Debug (Tree_f Int))       
produce: Leaf
produce: Leaf
produce: Leaf
produce: Branch 3 <D Leaf> <D Leaf>
produce: Branch 1 <D Leaf> <D (Branch 3 <D Leaf> <D Leaf>)>
<D (Branch 1 <D Leaf> <D (Branch 3 <D Leaf> <D Leaf>)>)>
\end{verbatim}

\noindent
Like for paramorphisms we can create a specialized version that works for
annotation types that do not require a context to run in. We use the identity
monad to get back a pure annotated apomorphism.

> apoA :: AnnP a f Identity => Phi a f s -> s -> FixA a f
> apoA phi = runIdentity . apoMA phi

\noindent
Fixing the annotation to be the identity gives us back a pure apomorphism
working over structure without annotations.

> apo :: Traversable f => Phi Id f s -> s -> Fix f
> apo phi = apoA phi

\noindent
Now we can simply create a pure |fromList| version working on plain binary
trees without annotations.

> fromList :: [Int] -> Tree
> fromList = apo fromListCoalg

\end{subsection}

\begin{subsection}{Endomorphic paramorphism}

Both the paramorphisms and the apomorphisms working on annotated structures had
enough information to know when to use the |query| or |produce| function to
read a structure from an annotation or to annotate a new structure. The
paramorphism starts out with querying the value from the annotation before
applying the algebra. The apomorphism produces an annotation returned by the
coalgebra.

The algebras as defiend in the previous sections are very general in the sense
that they can return a value of any result type |r|. Some paramorphisms might
choose to produce a value that is equal to the input value. An example of such
an algebra is the |replicate| algebra that replaces every value in a binary
tree with one and the same value.

> replicateAlg1 :: Int -> Psi1 a Tree_f (Fix1 Tree_f)
> replicateAlg1 _    Leaf                     = Leaf
> replicateAlg1 v (  Branch _ (_, l) (_, r))  = Branch v (In (Id l)) (In (Id r))

\noindent
The value that gets into the |replicateAlg| is of |FixA1 a (Tree_f v)|
for some arbitrary annotation. The algebra does not run in any context, and
should not run in any context, and cannot come up with new annotations. The
only thing it can do is fix the annotation type to |Id| and return a plain
binary tree as a result.
  
Because the type of |Psi| does not allow the reuse of existing sub-structure
with annotations in the output we have to create a new type of albera for
\emph{endomorphic paramorphisms}, paramorphisms for which the result type
equals the output type.

> type Endo a f = f (FixA a f :*: FixA a f) -> (FixA a f :+: FixA1 a f)

\noindent
The |Endo| type is an specialized version of the |Psi| type and describes an
algbera that returns either an existing fully annotated structure or produces a
new structure with existing fully annotated sub-structures. 

> endoMA :: AnnM a f m => Endo a f -> FixA a f -> m (FixA a f)
> endoMA psi = (return `either` produce) . psi <=< mapM (group (endoMA psi)) <=< query
>   where group f c = fmap ((,) c) (f c)

\noindent
The only real difference between the |paraMA| and the |endoMA| function is that
the latter knows it might need to use the |produce| function on the result of
the algbera |psi|. The |paraMA| function can be used to compute any results
value from an input structure, even unannotated forms of the input structure.
The |endoMA| function can be used to compute a fully annotated structure with
the same type as the input structure.

We can now rewrite the |replicateAlg| algbera to produce annotated structures.

> replicateAlg :: Int -> Endo a Tree_f
> replicateAlg _    Leaf                     = Right Leaf
> replicateAlg v (  Branch _ (_, l) (_, r))  = Right (Branch v l r)

\noindent
Because the |replicateAlg| produces a new structure and does not directly
resuses pre-annotated sub-structure it uses the |Right| constructor from the
sum-type. The |endoMA| morphism now know to provide annotations for these new
structures using the |produce| function.

Combinging the endomorphic paramorphism with the algbera for replication gives
us back a true replicate function for annotated structures.

> replicateMA :: AnnM a Tree_f m => Int -> FixA a Tree_f -> m (FixA a Tree_f)
> replicateMA v = endoMA (replicateAlg v)

\noindent
We can now test this function on the result of our prevouis example, the
|fromListMA [1, 3]|. The result shows a nice debug trace of how the
|replicateMA| function traverses the binary tree and builds it up again using
the replicated value.

\begin{verbatim}
ghci> replicateMA 4 it
query: Branch 1 <D Leaf> <D (Branch 3 <D Leaf> <D Leaf>)>
query: Leaf
produce: Leaf
query: Branch 3 <D Leaf> <D Leaf>
query: Leaf
produce: Leaf
query: Leaf
produce: Leaf
produce: Branch 4 <D Leaf> <D Leaf>
produce: Branch 4 <D Leaf> <D (Branch 4 <D Leaf> <D Leaf>)>
<D (Branch 4 <D Leaf> <D (Branch 4 <D Leaf> <D Leaf>)>)>
\end{verbatim}

\noindent
Like for regular paramorphisms we can create a specialized version that works for
annotation types that do not require a context to run in. We use the identity
monad to get back a pure annotated endomorphic paramorphism.

> endoA :: AnnM a f Identity => Endo a f -> FixA a f -> FixA a f
> endoA psi = runIdentity . endoMA psi

\noindent
Fixing the annotation to be the identity gives us back a pure endomorphic
paramorphism working over structure without annotations.

> endo :: Traversable f => Endo Id f -> Fix f -> Fix f
> endo psi = endoA psi

\end{subsection}

\begin{subsection}{Endomorphic apomorphisms}

Similar to the concept of endomorphic paramorphism are the endomorphic
apomorphisms. Endomorphic paramorphisms are specific apomorphisms in the sense
that the input seed to produce new structures from is itself of the same
structure type. Endomorphic apomorphisms working on annotated structures suffer
from the same problem their paramorphic counterparts, the (co)algberas do not
have enough information to reason about the annotation type.  This can
illustrated with the coalgebra for insertion into a binary tree.

\todo{this example might be a bit to undefined}

> insertCoalg1 :: Int -> Phi a Tree_f (Fix1 Tree_f)
> insertCoalg1 v s =
>  case s of
>    Branch w l r ->
>      case v `compare` w of
>        LT  -> Branch w  (Left (unId (out l)))  (Right undefined)
>        EQ  -> Branch v  (Right undefined)      (Left (unId (out r)))
>        GT  -> Branch w  (Right undefined)      (Right undefined)
>    Leaf    -> Branch v  (Right undefined)      (Right undefined)

Remeber that the coalgebra for apomorphisms could decide whether to produce a
new seed or a new sub-structure directly using the |Left| and |Right|
constructors of the sum datatype. The |undefined| symbols (Haskells
\emph{undefined}) in the |insertCoalg| are the places that cannot be filled in
because the coalgebra cannot inspect annotated structures. To allow writing
proper endomorphic coalgebras for annotated structure we introduce a new
coalgbera type that can exploit more information.

> type CoEndo a f = f (FixA a f) -> f (FixA a f :+: (FixA a f :+: f (FixA a f)))

The |CoEndo| type signature fixes the input seed to the type of the structure
that will be produced. The output structure can have three different types of
value at the sub-positions, hence the nested sum type. 

\begin{itemize}
\item The first is a new seed to recursively continue.
\item The second means stopping and reusing pre-annotated structure.
\item The third means stopping and invention structure with pre-annotated sub-structures.
\end{itemize}

Using the |CoEndo| type for endomorphic coalgberas we can rewrite the
|insertCoalg| to take advantage of this and write a true insertion coalgbera.

> insertCoalg :: Int -> CoEndo a Tree_f
> insertCoalg v s =
>   case s of
>     Branch w l r ->
>       case v `compare` w of
>         LT  -> Branch w  (Left l)              (Right (Left r))
>         EQ  -> Branch v  (Right (Left l))      (Right (Left r))
>         GT  -> Branch w  (Right (Left l))      (Left r)
>     Leaf    -> Branch v  (Right (Right Leaf))  (Right (Right Leaf))

The |coendoMA| function takes a endomorphic coalgbera and an fully annotated
input structure and produces an fully annotated output structure. Note that
both the endomorphic paramorphism and the endomorphic apomorphism modify an
input structure to an output structure of the same type. Where the endomorphic
paramorphisms takes in input algberas and the endomorphic apomorphisms take
coalgberas.

> coendoMA :: (Traversable f, AnnM a f m) => CoEndo a f -> FixA a f -> m (FixA a f)
> coendoMA phi = modify (mapM c . phi)
>   where
>   c (Left x)           = coendoMA phi x
>   c (Right (Left  x))  = return x
>   c (Right (Right x))  = produce x

The |coendoMA| morphism applies the coalgbera |phi| to the annotated input
structure throught the use of the |modify| function from the |AnnM| type class.
The |modify| function makes sure the input structure is queried from the root
annotation and will be supplied a new annotation after applying the specified
function. After applying the coalgbera |phi| a case analysis will be done on
the result. The three possible cases of the nested sum type will be: going into
corecursion with a new seed value, reusing an existing fully annotated
structure, or producing a new structure while reusing the fully annotated
sub-structures.

Combining the endomorphic apomorphism with the endomorphic coalgbera for binary
tree insertion gives us back a true insert function on annotated binary trees.

> insert :: AnnM a Tree_f m => Int -> FixA a Tree_f -> m (FixA a Tree_f)
> insert v = coendoMA (insertCoalg v)

We can test this by inserting the value |0| into the example tree binary tree
produced before with the |fromList [1, 2]|. The debug trace shows the traversal
that is being performed.

\begin{verbatim}
ghci> insert 0 it
query: Branch 1 <D Leaf> <D (Branch 3 <D Leaf> <D Leaf>)>
query: Leaf
produce: Leaf
produce: Leaf
produce: Branch 0 <D Leaf> <D Leaf>
produce: Branch 1 <D (Branch 0 <D Leaf> <D Leaf>)>
                  <D (Branch 3 <D Leaf> <D Leaf>)>
<D (Branch 1 <D (Branch 0 <D Leaf> <D Leaf>)>
             <D (Branch 3 <D Leaf> <D Leaf>)>)>
\end{verbatim}

\noindent
And additionally, in the line of the other morphisms defined, two
specializations of this endomorphic apomorphism. The |coendoA| works for
annotations not requiring any context, the |coendo| working on pure structures
not requiring any context or annotation.

> coendoA :: AnnM a f Identity => CoEndo a f -> FixA a f -> FixA a f 
> coendoA phi = runIdentity . coendoMA phi

> coendo :: Traversable f => CoEndo Id f -> Fix f -> Fix f
> coendo phi = coendoA phi

\end{subsection}

\begin{subsection}{Applicative paramorphisms}

When writing modification functions over datatypes using morphisms like the
ones defined above it is not always easy to compose small functions into bigger
ones. Combining multilpe algebras into a single one that can be used in a
single traversal is a well known problem in the world of functional
programming. Attribute grammar systems can be used to combine different
algebraic operations into a single tree traversal in an aspect-oriented way.

This section describes a more lightweight and idiomatic approach to algebra
composition. We make the paramorphic algebra type an instance of where
Haskell's applicative functors type class.

First we change the type for the algebra |Psi| from a type synonym into a real
datatype using a GADT. We do so to be able to add additional constructor as we
will later see is needed for the applicative instance. 

> data Psi (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
>   Alg  :: (f (FixA a f, r) -> r)  -> Psi a f r

%if False

>   Prj  :: Psi a f (r -> s, r, s)  -> Psi a f s

%endif

\noindent
The applicative type class requires us to have a |Functor| instance and to
implement two functions, |pure| and |<*>|.

\begin{spec}
class Functor f => Applicative f where
  pure   :: f a
  (<*>)  :: f (a -> b) -> f a -> f b
\end{spec}

\noindent
When we unify the type |f| with the the |Psi| type we get the following
function type we have to support when implementing applicative algebras.

\begin{spec}
Psi c f (a -> b) -> Psi c f a -> Psi c f b
\end{spec}

\noindent
From this signature it is clear that combining an algebra that produces a
function from |a -> b| and an algebra that produces a |a| into an algebra that
produces only a |b| throws away any information about |a|. Because both the
function and the argument are needed \emph{in every stage of the traversal} we
first create a function |<++>| that groups together both the function, the
argument and the result.

> (<++>) :: Functor f => Psi a f (r -> s) -> Psi a f r -> Psi a f (r -> s, r, s)
> Alg  f <++> Alg  g = Alg (\x -> f (fmap (fmap fst3) x) `mk` g  (fmap (fmap snd3) x))
>   where mk x y = (x, y, x y)

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

\noindent
Now we add an additional constructor to the |Psi| datatype that projects the
last component from the triple algebra created by the |<++>| function. It uses
the |Psi| GADT to create an existential quantificaiton that hide the type
variable |r| inside the |Prj| constructor.

\begin{spec}
Prj :: Psi a f (r -> s, r, s)  -> Psi a f s
\end{spec}

\noindent
The new constructor requires us the to extend the grouping function with the
three additional cases involving the projection constructor. The implementation
is quite straightforward but note that it uses the |pure| function from the
applicative instance we do not yet have defined.

\begin{spec}
Prj  f <++> Prj  g = fmap trd3 f <++> fmap trd3 g 
Alg  f <++> Prj  g = Prj (pure id <++> Alg f) <++> Prj g
Prj  f <++> Alg  g = Prj f <++> Prj (pure id <++> Alg g)
\end{spec}

\noindent
With the both the grouping function and the projection function we have enough
tools to easily create the applicative instance for paramorphic algebras. The
applicative sequencing is a matter of grouping the input and output information
together and then only throwing the input types away using the existential from
the GADT.

> instance Functor f => Applicative (Psi a f) where
>   pure    = Alg . const
>   a <*> b = Prj (a <++> b)

\noindent
We require a |Functor| super instance for this applicative instance which can
easily be done more or less the same way as the applicative instance by lifting
the function to fmap using |pure|.

> instance Functor f => Functor (Psi a f) where
>   fmap f psi = Prj (pure f <++> psi)

Using the applicative instance we can now easily compose multiple algebras into
one to be able to apply them to an input structure in one traversal. This
composability can really help us to create more complicated algebras without
creating very big tuples representing the all the components at once.

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

> paraMA :: (Lazy m, AnnQ a f m) =>  Psi a f r -> FixA a f -> m r
> paraMA (Prj  psi) = fmap trd3 . paraMA psi
> paraMA (Alg  psi) = return . psi <=< mapM (g (lazy . paraMA (Alg psi))) <=< query
>   where g f c = fmap ((,) c) (f c)

The implementation of the projection aware |paraMA| is not very exciting and
quite similar to the |paraMA| defined previously. The only conceptual
difference is that for the |Prj| constructor the function unpacks the
existential and recursively applies the paramorphism. 

\end{subsection}

\begin{subsection}{Lazy IO and strict paramorphisms}

As decribed in section TODO. 

The |paraMA| function implemented earlier is as strict as the context it is
executed in. The context is dependent on the annotation type, for example, the
debug annotation works in |IO| which makes it strict. This strictness has the
implication that the paramorphism will compute all the sub-results for all
sub-structures independent of the evetual usage. This is the reason that the
debug annotation in example TODO prints out from sub-trees than one would
expect from a lazy in-memory paramorphic traversal.

One of the things this section shows how we can make paramorphisms more lazy on
the inside, making sure only sub-structures are traversed when they are needed
for the eventual output. Some monads are lazy by default like the |Identity|
monad and the |Reader| monad. Using these as the annotation context would make
the traversal naturally lazy. Some other monads are strict by default requiring
all computations on the left hand side of the monadic bind to happen strictly
before the right hand side can be executed. Example of these monads are the
|IO| monad and the |State| monad. 

The decission about what sub-structures are needed for a certain computation is
up to the algebra and to the paramorphism function itself. The algebra is a
pure description that is unaware of the annotation or associated context.
Because the paramorphism function does not know what information is used by
the algebra it has to pass in all the recursive sub-results. To clarify this we
can look at the |containsAlg| for binary trees. As input this algebra get a
single structure with two booleans in the sub-structures, these booleans are
the indication whether the value is included in one of the sub-structures.
Because we are dealing with a lazy language these sub-results are ideally not
computed yet, and will only be used when the algebras desires so. Running the
paramorphic traversal inside a strict context, like |IO| for our debug
annotation, will actually strictly precompute the recursive results. This
changes the running time for the |containsMA| function from the expected |O(log
n)| to an unacceptable |O(n)|.

To solve this problem we introduce a type class |Lazy| that should allow use to
explicitly turn strict monads into lazy ones where possible. The only class
method is the function |lazy| that gets a monadic computation and turns it into
an lazy variant of this computation. Off course this will not be possible in
the most general case for all monads.

> class AM m => Lazy m where
>   lazy :: m a -> m a

Both the |Identity| monad and the |Reader| monad are lazy and can trivially be
made an instance of this type class. To be a bit more general we make the
|ReaderT| monad transformer an instance of the |Lazy| class for all cases that
it transforms another lazy monad.

> instance Lazy Identity where
>   lazy = id

> instance Lazy m => Lazy (ReaderT r m) where
>   lazy c = ask >>= lift . lazy . runReaderT c

We can also make |IO| an instance of the |Lazy| class by using the
|unsafeInterleaveIO| function. This function takes an |IO| computation and
produces an |IO| computation that will only be performed when the result is
needed. This breaks the strict semantics of the |IO| monad, but can become
useful for our case.

> instance Lazy IO where
>   lazy = unsafeInterleaveIO

Now we have a way to enforce lazy semantics for some of the contexts our
traversals might run. By explicitly putting a call to |lazy| just before the
recursive invocations in the paramorphism we can make the entire traversal
lazy.

> lazyParaMA :: (Lazy m, AnnQ a f m) => Psi1 a f r -> FixA a f -> m r
> lazyParaMA psi = return . psi <=< mapM (group (lazy . lazyParaMA psi)) <=< query
>   where group f c = fmap ((,) c) (f c)

When we now express the |containsMA| in terms of the more lazy paramorphism.

> containsMA2 :: (Lazy m, AnnQ a Tree_f m) => Int -> FixA a Tree_f -> m Bool
> containsMA2 v = lazyParaMA (containsAlg v)

Now running the |containsMA| on our example tree created by |fromList [1, 2]|
shows that the debug trace only prints out the first element of the binary
tree, because no other sub-results are needed to come up with the answer.

\begin{verbatim}
ghci> containsMA2 3 it
query: Branch 3 <D Leaf> <D Leaf>
True
\end{verbatim}

% todo: ref something about evility of lazy IO.
% todo: deepseq rerefence

\noindent
The GHCi debugger by default prints out the value the user request at the
prompt, this is the only reason the expression |containsMA2 3 it| is evaluated
at all. The traversal has become become lazy on the inside, no unneeded
traversals will be performed, but also lazy on the outside, not a thing will
happen until the answer is forced. This behaviour can be compared to the
|hGetContents| function from the Haskell prelude which uses lazy |IO| (also
using |unsafeInterleaveIO|) to give back the entire contents of a file or
socket resource as a single lazu |String|. This means the actual |IO| actions
to read the contents from file will only be performed when the individual
characters of the string will be inspected. This can have a strange outcome in
practice, evaluation of pure code can have real world side effects. The same is
the case for our lazy traversals, the debug annotations only kicks in when the
result is inspected. While this behaviour is probaly safe for the debug
annotation this might not be the case in general.
  
To make sure all the potential side effect stay within the context they are
restricted to we have to make sure our paramorphism remains strict from the
outside. We do this by deep forcing the result of the evaluation before
returning the answer. Using generic programming we have implemented a generic
deep |seq| function that can be used to force an entire computation fully and
recursively the moment it would normally only get forced WHNF. The
implementation internally uses the |seq| function from the Haskell prelude that
forces the first argument WHNF when the second argument gets forced.

> dseq1  :: a -> a
> seq1   :: a -> b -> b

%if False

> dseq1 = undefined
> seq1 = undefined

%endif

\noindent
By creating a new paramorphism function that forces the result before returing
we get a traversal that is lazy internally but is strict on the outside. As we
will see later when dealing with data persistency this evaluation semantics is
essential.

> paraMA' :: (Lazy m, AnnQ a f m) => Psi1 a f r -> FixA a f -> m r
> paraMA' psi = dseq1 `liftM` lazyParaMA psi

\end{subsection}

\end{section}

