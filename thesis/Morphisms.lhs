%include polycode.fmt

%if False

> {-# LANGUAGE KindSignatures, UndecidableInstances #-}
> module Morphisms where

> import Control.Applicative
> import Control.Category
> import Control.Monad.Identity
> import Control.Monad.Reader hiding (mapM)
> import System.IO.Unsafe
> import Data.Foldable
> import Data.Monoid hiding (Endo)
> import Data.Traversable
> import Data.Traversable
> import Prelude hiding ((.), id, mapM)
> import Data.Time.LocalTime

%endif

%format mempty    = "\varnothing"
%format `mappend` = "\diamond"
%format fmap' a   = "\widehat{" a "}"
%format <*>       = "\circledast"
%format <=<       = "\triangleleft"
%format >>=       = "\rightarrowtail"
%format fixp      = "fix"

%format :*: = "\times"
%format :+: = "+"

%format AM = "Monad"

%if False

> fmap' :: Functor f => (a -> b) -> f a -> f b
> fmap' = fmap

> infixl 6 :+:
> infixl 7 :*:

> type a :+: b = Either a b
> type a :*: b = (a, b)

> class (Applicative m, Monad m) => AM m
> instance (Applicative m, Monad m) => AM m

> fixp :: (t -> t) -> t
> fixp a = a (fixp a)

%endif

\section{Generic traversals with annotations}

\subsection{Fixed points}

%format Fix_1     = "\mu"
%format In_1      = "In"
%format out_1     = "out"

%format Tree_1    = "Tree"
%format Leaf_1    = "Leaf"
%format Branch_1  = "Branch"

%format Tree_f    = "Tree_f"

%format Tree_2    = "Tree"
%format Leaf_2    = "Leaf"
%format Branch_2  = "Branch"

%format mapM1 = "mapM"

In Haskell, most container datatypes are written down with explicit recursion.
An example of explicit recursion is the following binary tree datatype. This
binary tree stores both a value and two explicit sub-trees in the branch
constructor, empty trees are indicated by a leaf.

> data Tree_1 v = Leaf | Branch_1 v (Tree_1 v) (Tree_1 v)

To gain more control over the recursive positions of the datatype we can
parametrize the binary tree with an additional type parameter used at the
recursive positions. Not the tree datatype itself, but the users of the
datatype mow decide what values to store as sub-trees.

> data Tree_f v f = Leaf_2 | Branch_2 v f f

To get back a binary tree that is isomorphic to our original binary tree that
stored actual sub-trees at the recursive points we can use an explicit fixed
point combinator at the type level. This combinator, conveniently called
|Fix_1|, takes a type constructor of kind |* -> *| and parametrizes this type
with its own fixed point.

> newtype Fix_1 (f :: * -> *) = In_1 { out_1 :: f (Fix_1 f) }

By applying the fixed point combinator to the |Tree_f| datatype we get a back a
real binary tree again with real sub-trees at the recursive positions.

> type Tree_2 v = Fix_1 (Tree_f v)

Because of the use of the |newtype| |Fix_1| all sub-tree constructors will be now
surrounded by an additional |In| constructor. To make the usage of binary trees
more easy we create two smart constructors: |leaf| and |branch|.

> leaf :: Tree_2 v
> leaf = In_1 Leaf_2
>
> branch :: v -> Tree_2 v -> Tree_2 v -> Tree_2 v
> branch v l r = In_1 (Branch_2 v l r)

To make it easier to deal with the recursive structure of the binary tree we
can make the |Tree_f| an instance of Haskell's |Functor| type class. The
functorial |fmap| lifts the function to be applied against the sub-trees of the
binary tree.

> instance Functor (Tree_f v) where
>   fmap _  Leaf_2            = Leaf_2
>   fmap f  (Branch_2 v l r)  = Branch_2 v (f l) (f r)

Besides |Functor| Haskell has two additional type classes that help with
generic traversals over container data types. These are the |Foldable| and
|Traversable| type classes\footnote{Note that these type class instances are
very simple and mechanically derivable. The GHC Haskell compiler version 6.12.1
and above is able to derive the instances for |Functor|, |Foldable| and
|Traversable| for you automatically.}. The foldable type class allows us to
reduce an entire structure into a single value using some |Monoid| operation. 

> instance Foldable (Tree_f v) where
>   foldMap _  Leaf_2            = mempty
>   foldMap f  (Branch_2 _ l r)  = f l `mappend` f r

The |Traversable| class allows us to traverse the structure from left to right
and perform an actions for each element.

> instance Traversable (Tree_f v) where
>   traverse _  Leaf_2            = pure Leaf_2
>   traverse f  (Branch_2 v l r)  = pure (Branch_2 v) <*> f l <*> f r

The |Traversable| is very useful because it allows us to use the generic
version of the Prelude's |mapM| function. This function allows us to |fmap| a
monadic action over a structure and transpose the result:

> mapM1 :: (Traversable f, AM m) => (a -> m b) -> f a -> m (f b)

%if False

> mapM1 = undefined

%endif

\subsection{Annotations}

%format FixA  = "\mu_{\alpha}"
%format FixA1 = "\mu_{\alpha}^1"
%format FixA2 = "\mu_{\alpha}^2"
%format Fix   = "\mu"
%format Fix1  = "\mu^1"
%format Fix2  = "\mu^2"

In the previous we worked out some basic building blocks that can be useful
when working with container data types with the recursive point parametrized.
But why would it be useful to abstract away from the recursive points in the
first place? This section show how we can store additional information in the
recursive points using an annotated fixed combinator.

First we introduce an new fixed combinator that stores an annotation over the
container data type instead of a value of the data type itself. This type level
fixed point combinator is called |FixA|, the \emph{alpha} postfix indicates it
can store arbitrary stacks of annotations at the recursive positions of the
structure it contains.

> newtype FixA a f = In { out :: (a f) (FixA a f) }

Note the kind of the annotation variable |a|, the annotation is applied over
the original container which has kind |* -> *|, because the annotation itself
needs to have the same kind the type variable |a| has kind |(* -> *) -> (* -> *)|.

Sometimes it is easier for functions to work with a structure with fully
annotated sub-structures. We create a type synonym |FixA1| that represents
this.

> type FixA1 a f = f (FixA a f)

Sometimes it is easier for functions to work with a structure with annotated
fixed points without having the first |In| constructor around, directly
exposing the annotation value. We use the following type synonym -- which is
isomorphic to |FixA1| --  for this:

> type FixA2 a f = (a f) (FixA a f)

We now introduce the identity annotation, called |Id|, that stores no
additional information but just encapsulates the underlying container type.

> newtype Id f a = Id { unId :: f a }

The identity annotation can be used to get back the regular fixed point
combinator defined in the previous section by plugging it into a |FixA|.

> type Fix f = FixA Id f

We also introduce a type synonym |Fix1| similar to the |FixA1| and |Fix2|
similar to |FixA2|, both aliases for the usage of identity annotations.

> type Fix1  f =      f   (Fix f)
> type Fix2  f = (Id  f)  (Fix f)

The annotated fixed points can be used to store arbitrary pieces of data at the
recursive points of a recursive structure. For example, when you want a binary
tree annotated with the times at which some sub-tree was last modified you
could write something like this:

> data TimeAnn f a = TA LocalTime (f a)
> type TimedTree v = FixA TimeAnn (Tree_f v)

\subsection{Annotation associated functionality}

%format AnnQ = "Ann_Q"
%format AnnP = "Ann_P"
%format AnnM = "Ann_M"

In the previous section we showed how to store arbitrary pieces of information
at the recursive points of a data type. In this section we will show how to
associate functionality to these annotations. For every annotation type we will
describe how to obtain an annotation for a certain recursive point, we will
call this the |produce| function and how to get the recursive structure back
from the annotation, we call this the |query| function. The following type
signatures describe these two actions.

> type Produce  a f m  =      f (  FixA   a f)   -> m (     FixA   a f)
>
> type Query    a f m  =           FixA   a f    -> m (f (  FixA   a f))

As the type signature shows, a producer will take a structure with an annotated
substructure and introduces a new annotation for this structure. The function
might run in some context |m| when this is required for the annotation. The
type signature for queries shows that it will take an annotated structure and
will use the annotation to give back the structure itself. This functions can
also run in the context |m|.

Two type classes are used to associate specific functionality to an annotation,
for queries this class is called |AnnQ|, for producers this class is called
|AnnM|. They both contains a single function with the type signature defined
above. The first parameter of the type class |a| is the annotation type, the
second parameter |f| is the structure to annotate, the third |m| is the context
it may run in.

> class (Traversable f, AM m) => AnnQ a f m where
>   query :: Query a f m

> class (Traversable f, AM m) => AnnP a f m where
>   produce :: Produce a f m

Making an annotation type instance of this class means we can come up with an
annotation for a structure and can get back to the structure again. Note that
the |Traversable| and the |Monad| classes in the context are not strictly
necessary here. These super classes only help to prune the contexts when using
the |AnnQ| and |AnnM| classes. 

In all the examples that follow we assume that the notion of |Monad| in
a type context also assume the existinence of an |Applicative| instance.
Although this assumption is not strictly the case in Haskell it is valid in
theory and saves us some typing.

Now the instances for the identity annotation is very easy, we just unpack and
pack the annotation and strips off or introduces the |In| constructor.

> instance (Traversable f, AM m) => AnnQ Id f m where
>   query = return . unId . out

> instance (Traversable f, AM m) => AnnP Id f m where
>   produce = return . In . Id

Although redundant in the general case, for possible optimizations we also
introduce a type class for modification of a sub-structure, called |AnnM|. The
|modify| function is used to apply a function over an annotated structure.
There is a default implementation available which is just the Kleisli
composition (denoted by |<=<|) of the query, the function, and the producer.

> type Modify   a f m  =   (  f (  FixA   a f)   -> m (f (  FixA   a f)))
>                      ->  (       FixA   a f    -> m (     FixA   a f))

> class (AnnQ a f m, AnnP a f m) => AnnM a f m where
>   modify :: Modify a f m
>   modify f = produce <=< f <=< query

For the identity we just use the default implementation.

> instance (Traversable f, AM m) => AnnM Id f m

\subsection{Paramorphisms}

%format paraMA1 = "para^m_{\alpha}"
%format paraM1  = "para^m"
%format paraA1  = "para_{\alpha}"
%format para1   = "para"
%format psi     = "\psi"
%format Psi1    = "\Psi"
%format Psi2    = "\Psi"
%format Psi     = "\Psi"

Now we have a way to associate functionality with annotations we should able to
write operations over our annotated structures -- like the binary tree example
-- and perform actions each time we would normally go directly into recursion.
To generalize this pattern we abstract away from recursion why writing the
structure-processing algorithms using morphisms. We start out with the
\emph{paramorphism}, which is a generalization of the \emph{catamorphism}, a
bottom up traversal that can fold an entire structure into a single value.

We first write down the type signature of the algebra for the paramorphism, we
call this algebra |Psi1|.  This algebra is a function from a functor containing
both the original substructure and the recursive result to a new result.

> type Psi1 a f r = f (FixA a f :*: r) -> r

The paramorphism is a function that performs a bottom

> paraMA1 :: AnnQ a f m => Psi1 a f r -> FixA a f -> m r
> paraMA1 psi = return . psi <=< mapM (sub (paraMA1 psi)) <=< query
>  where sub f c = fmap ((,) c) (f c)

> paraM1 :: (AM m, Traversable f) => Psi1 Id f r -> Fix f -> m r
> paraM1 psi = paraMA1 psi

> paraA1 :: (AnnQ a f Identity, Traversable f) => Psi1 a f c -> FixA a f -> c
> paraA1 psi = runIdentity . paraMA1 psi

> para1 :: Traversable f => Psi1 Id f c -> Fix f -> c
> para1 psi = runIdentity . paraM1 psi

\subsection{Apomorphisms}

%format apoMA = "apo^m_{\alpha}"
%format apoM  = "apo^m"
%format apoA  = "apo_{\alpha}"
%format apo   = "apo"
%format phi   = "\phi"
%format Phi   = "\Phi"

> type Phi a f s = s -> f (s :+: f (FixA a f))

> apoMA :: (Functor m, Traversable f, AnnP a f m) => Phi a f s -> s -> m (FixA a f)
> apoMA phi = produce <=< mapM (apoMA phi `either` produce) . phi

> apoM :: (Functor m, Traversable f, AnnP Id f m) => Phi Id f s -> s -> m (Fix f)
> apoM = apoMA

> apoA :: (Traversable f, AnnP a f Identity) => Phi a f s -> s -> FixA a f
> apoA phi = runIdentity . apoMA phi

> apo :: Traversable f => Phi Id f s -> s -> Fix f
> apo phi = runIdentity . apoM phi

\subsection{Lazy IO and strict paramorphisms}

> class Lazy m where
>   lazy :: m a -> m a

> instance Lazy Identity where
>   lazy = id

> instance (AM m, Lazy m) => Lazy (ReaderT r m) where
>   lazy c = ask >>= lift . lazy . runReaderT c

> instance Lazy IO where
>   lazy = unsafeInterleaveIO

> dseq :: a -> a

%if False

> dseq = undefined

%endif

> para' :: (Lazy m, AnnQ a f m) => Psi1 a f r -> FixA a f -> m r
> para' psi = fmap' dseq (fix (\pm -> return . psi <=< mapM (sub (lazy . pm)) <=< query))
>   where sub f c = fmap ((,) c) (f c)

\subsection{Applicative paramorphisms}

%format <++> = "\oplus"
%format paraMA = "para^m_{\alpha}"
%format elipses = "\dots"

> data Psi (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
>   Alg  :: ((f r, f (FixA a f)) -> r)  -> Psi a f r
>   Prj  :: Psi a f (r -> s, r, s)      -> Psi a f s

> instance Functor f => Functor (Psi a f) where
>   fmap f psi = Prj (pure f <++> psi)

> instance Functor f => Applicative (Psi a f) where
>   pure     = Alg . const
>   a <*> b  = Prj (a <++> b)

%if False

> idPsi :: Functor f => Psi a f (r -> r)
> idPsi = pure id

> fst3 :: (a, b, c) -> a
> fst3 (x, _, _) = x

> snd3 :: (a, b, c) -> b
> snd3 (_, y, _) = y

> trd3 :: (a, b, c) -> c
> trd3 (_, _, z) = z

%endif

> (<++>)  :: (Functor f, Functor (Psi a f)) => Psi a f (r -> s) -> Psi a f r -> Psi a f (r -> s, r, s)
> Alg  f <++> Prj  g = Prj (idPsi <++> Alg f) <++> Prj g
> Prj  f <++> Alg  g = Prj f <++> Prj (idPsi <++> Alg g)
> Prj  f <++> Prj  g = fmap' trd3 f <++> fmap' trd3 g 
> Alg  f <++> Alg  g = Alg (\(a, b) -> f (fmap' fst3 a, b) `mk` g (fmap' snd3 a, b))
>   where mk x y = (x, y, x y)

> paraMA :: (Traversable f, Lazy m, Functor m, AnnQ a f m) => Psi a f r -> FixA a f -> m r
> paraMA (Prj psi) f = fmap' trd3 (paraMA psi f)
> paraMA (Alg psi) f = elipses

%if False

>   where elipses =
>           do g <- query f
>              r <- mapM (lazy . paraMA (Alg psi)) g
>              return (psi (r, g))

%endif

\subsection{Endomorphic paramorphism}

%format Endo    = "\Psi_{endo}"
%format endoMA  = " endo^m_{\alpha}"
%format endoMA' = "helper"
%format endoM   = "endo^m"
%format endoA   = "endo_{\alpha}"
%format endo    = "endo"

> type Endo a f = Psi a f (FixA a f :+: f (FixA a f))

> toEndo :: Functor f => Psi a f (FixA a f) -> Endo a f
> toEndo = fmap' Left

> endoMA  :: (Functor m, Lazy m, AnnQ a f m, AnnP a f m)
>         => Endo a f -> FixA a f -> m (FixA a f)
> endoMA psi = endoMA' (return `either` produce) Left psi

> endoMA'  :: (Functor m, Lazy m, AnnQ a f m)
>          => (x -> m r) -> (r -> x) -> Psi a f x -> FixA a f -> m r
> endoMA' z y (Alg psi) f = 
>   do  g   <- query f
>       r   <- fmap' y `fmap` mapM (lazy . endoMA' z y (Alg psi)) g
>       z (psi (r, g))
> endoMA' z y (Prj psi) f = fmap' trd3 (endoMA' f0 f1 psi f)
>     where  f0  (a, b, r) = z r >>= \r' -> return (a, b, r')
>            f1  (a, b, r) = (a, b, y r)

> endoM :: (Traversable f, Lazy m, AM m) => Endo Id f -> Fix f -> m (Fix f)
> endoM psi = endoMA psi

> endoA :: (AnnQ a f Identity, AnnP a f Identity) => Endo a f -> FixA a f -> FixA a f
> endoA psi = runIdentity . endoMA psi

> endo :: Traversable f => Endo Id f -> Fix f -> Fix f
> endo psi = runIdentity . endoM psi

\subsection{Endomorphic apomorphisms}

%format CoEndo    = "\Phi_{endo}"
%format coendoMA  = " coendo^m_{\alpha}"
%format coendoM   = "coendo^m"
%format coendoA   = "coendo_{\alpha}"
%format coendo    = "coendo"

> type CoEndo a f = f (FixA a f) -> f (FixA a f :+: (FixA a f :+: f (FixA a f)))

> coendoMA :: (Traversable f, AnnM a f m) => CoEndo a f -> FixA a f -> m (FixA a f)
> coendoMA phi = modify (mapM cont . phi)
>   where
>   cont (Left x)           = coendoMA phi x
>   cont (Right (Left  x))  = return x
>   cont (Right (Right x))  = produce x

> coendoM :: (Traversable f, AM m) => CoEndo Id f -> Fix f -> m (Fix f) 
> coendoM = coendoMA

> coendoA :: (AnnM a f Identity) => CoEndo a f -> FixA a f -> FixA a f 
> coendoA phi = runIdentity . coendoMA phi

> coendo :: Traversable f => CoEndo Id f -> Fix f -> Fix f
> coendo phi = runIdentity . coendoM phi

