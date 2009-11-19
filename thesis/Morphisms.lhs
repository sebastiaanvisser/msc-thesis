%include polycode.fmt
%include thesis.fmt

%if False

> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, UndecidableInstances #-}
> module Morphisms where

> import Control.Applicative
> import Control.Category
> import Control.Monad.Identity
> import Control.Monad.Reader hiding (mapM)
> import System.IO.Unsafe
> import Data.Traversable
> import Prelude hiding ((.), id, mapM, lookup)
> import Fixpoints

%endif

\begin{section}{Generic traversals with annotations}

\begin{subsection}{Paramorphisms}

Now we have a way to associate functionality with annotations we should able to
write operations over our annotated structures -- like the binary tree example
-- and perform actions each time we would normally go directly into recursion.
To generalize this pattern we abstract away from recursion when writing the
structure-processing algorithms using morphisms. We start out with the
\emph{paramorphism}, which is a generalization of the \emph{catamorphism}, a
bottom up traversal that can fold an entire structure into a single value.

We first write down the type signature of the algebra for the paramorphism, we
call this algebra |Psi1|. 

> type Psi1 a f r = f (FixA a f :*: r) -> r

\noindent
This type signature describes that an algebra should be able to produce an
value of type |r| from one non-recursive pieces of a recursive structure, all
the recursive sub-results of the computation and the original sub-structures.

An example of such an algebra is the |containsAlg| function for binary trees.
Because the algebra only uses the recursive sub-results and not the original
sub-structures this algebra is actually a catamorphism, a special case of the
more general paramorpism.

> containsAlg :: Ord v => v -> Psi1 a (Tree_f v) Bool
> containsAlg _  Leaf                      = False
> containsAlg v  (Branch c (_, l) (_, r))  = 
>   case v `compare` c of
>     LT  -> l
>     EQ  -> True
>     GT  -> r

\noindent
The paramorphism function performs a bottom up traversal over some |Functor|
and for every non-recursive piece applies the algebra. The most generic version
of this paramorphism within our annotation framework is the |paraMA1|. This
function runs is some monadic context |m| and performs a traversal over some
annotated structure |FixA a f| using the |AnnQ| type class to perform
annotation specific computations, hence the $(_{\alpha}^m)$ postfixes.

> paraMA1 :: AnnQ a f m => Psi1 a f r -> FixA a f -> m r
> paraMA1 psi = return . psi <=< mapM (group (paraMA1 psi)) <=< query
>   where group f c = fmap ((,) c) (f c)

\noindent
The implementation of this generic paramorphism might seem a bit cryptic at
first sight, this is due to its very generic behaviour. Quickly summarized this
function performs a bottom up traversal over a recursively annotation
structure. This functions gets a fully annotated structure as input and uses
the |query| function to get the original structure out of the annotation. The
|Traversable| instance that is an implicit super class of the |AnnQ| class
allows us to use the |mapM| function to recursively apply this |paraMA1|
function to the sub-structures to come up with the sub-results.  The
sub-results will be grouped together with the original sub-structures these
results are computed from. The original non-recursive piece of the input with
these grouped results in as the values will be passed into the algebra |psi|.
The algebra can now compute the result value for one level of the recursive
computation. 

To illustrate the usage of the |paraMA1| function we apply this paramorphism to
the |contains| algebra and get back a function that performs a |contains| over
an annotation binary tree.

> containsMA :: (Ord v, AnnQ a (Tree_f v) m) => v -> FixA a (Tree_f v) -> m Bool
> containsMA v = paraMA1 (containsAlg v)

\noindent
We can easily test this function in our interactive environment.
We first manually constructi a binary tree and constraining this to the |IO|
context and |Debug| annotation. While constructing the annotation prints out a
debug trace of all sub-structures being produced, exactly as defined.

\begin{verbatim}
ghci> join (branchA True <$> leafA <*> leafA) :: IO (TreeA Debug Bool)
produce: Leaf
produce: Leaf
produce: Branch True <D Leaf> <D Leaf>
<D (Branch True <D Leaf> <D Leaf>)>
\end{verbatim}

Now we can apply the |containsMA| function to the resulting binary tree and
check for the existinence of a |Branch| with value |True|. While running this
function the annotation print out a trace of all sub-structures being read from
the debug annotation.

\begin{verbatim}
ghci> containsMA True it
query: Branch True <D Leaf> <D Leaf>
query: Leaf
query: Leaf
True
\end{verbatim}

\noindent
When an annotation does not have any requirements about the type of context to
run in we can use the |Identity| monad to create a pure paramorphic traversal.

> paraA1 :: (AnnQ a f Identity, Traversable f) => Psi1 a f c -> FixA a f -> c
> paraA1 psi = runIdentity . paraMA1 psi

\noindent
When we further restrict the annotation to be the identity annotation we get
back a true pure paramorphism functions that works on plain in-memory data
structures.

> para1 :: Traversable f => Psi1 Id f c -> Fix f -> c
> para1 psi = paraA1 psi

\noindent
To illustrate this pure paramorphism we apply it to the |contains| algebra and
get back a pure |contains| function.

> contains :: Ord v => v -> Tree v -> Bool
> contains v = para1 (containsAlg v)

\end{subsection}

\begin{subsection}{Apomorphisms}

Dual to the paramorphism is the apomorphisms. Where the paramorphism abstract
away from recursion, the apomorphisms abstracts away from corecursion.
Similarly, apomorphisms use coalgebras to describe corecursive operations.
Apomorphisms are generalizations of anamorphisms, the |unfold| function that
can be used to create lists from a seed value is an example of an anamorphisms.

The coalgebra for an apomorphism, called |Phi|, takes a seed value of some type
|s| and should be able to produce an new seed or a recursive structure.

> type Phi a f s = s -> f (s :+: f (FixA a f))

\noindent
From the type signature of the |Phi| coalgebra it is obvious that it is dual to
the |Psi| algebra for paramorphisms. Paramorphisms destruct recursive
structures to some result value |r|, apomorphisms construct recursive
structures from some seed value |s|.

To illustrate the usage of this coalgebra we define the function
|fromListCoalg| that describeds how to create a balanced binary tree from an
input list. Note that because this coalgebra only produces new seeds (using the
|Left| constructor) instead of directly creating sub-structures it actually is
an anamorphism.

> fromListCoalg :: Phi a (Tree_f k) [k]
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

> apoMA :: (Functor m, AnnP a f m) => Phi a f s -> s -> m (FixA a f)
> apoMA phi = produce <=< mapM (apoMA phi `either` produce) . phi

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

> fromListMA :: AnnP a (Tree_f k) m => [k] -> m (FixA a (Tree_f k))
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

> apoA :: (AnnP a f Identity) => Phi a f s -> s -> FixA a f
> apoA phi = runIdentity . apoMA phi

\noindent
Fixing the annotation to be the identity gives us back a pure apomorphism
working over structure without annotations.

> apo :: Traversable f => Phi Id f s -> s -> Fix f
> apo phi = apoA phi

\noindent
Now we can simply create a pure |fromList| version working on plain binary
trees without annotations.

> fromList :: [k] -> Tree k
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

> replicateAlg1 :: Ord v => v -> Psi1 a (Tree_f v) (Fix1 (Tree_f v))
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

> replicateAlg :: forall a v. Ord v => v -> Endo a (Tree_f v)
> replicateAlg _    Leaf                     = Right Leaf
> replicateAlg v (  Branch _ (_, l) (_, r))  = Right (Branch v l r)

\noindent
Because the |replicateAlg| produces a new structure and does not directly
resuses pre-annotated sub-structure it uses the |Right| constructor from the
sum-type. The |endoMA| morphism now know to provide annotations for these new
structures using the |produce| function.

Combinging the endomorphic paramorphism with the algbera for replication gives
us back a true replicate function for annotated structures.

> replicateMA :: (Ord v, AnnM a (Tree_f v) m) => v -> FixA a (Tree_f v) -> m (FixA a (Tree_f v))
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

> insertCoalg1 :: Ord v => v -> Phi a (Tree_f v) (Fix1 (Tree_f v))
> insertCoalg1 v s =
>  case s of
>    Branch w l r ->
>      case v `compare` w of
>        LT  -> Branch w  (Left (unId (out l)))  (Right undefined)
>        EQ  -> Branch v  (Right undefined)      (Left (unId (out r)))
>        GT  -> Branch w  (Right undefined)      (Right undefined)
>    Leaf    -> Branch v  (Right Leaf)           (Right Leaf)

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

> coendoMA :: (Traversable f, AnnM a f m) => CoEndo a f -> FixA a f -> m (FixA a f)
> coendoMA phi = modify (mapM cont . phi)
>   where
>   cont (Left x)           = coendoMA phi x
>   cont (Right (Left  x))  = return x
>   cont (Right (Right x))  = produce x

> insertCoalg :: Ord v => v -> CoEndo a (Tree_f v)
> insertCoalg v s =
>   case s of
>     Branch w l r ->
>       case v `compare` w of
>         LT  -> Branch w  (Left l)              (Right (Left r))
>         EQ  -> Branch v  (Right (Left l))      (Left r)
>         GT  -> Branch w  (Right (Left l))      (Right (Left r))
>     Leaf    -> Branch v  (Right (Right Leaf))  (Right (Right Leaf))


> insert :: (Ord v, AnnM a (Tree_f v) m) => v -> FixA a (Tree_f v) -> m (FixA a (Tree_f v))
> insert v = coendoMA (insertCoalg v)

> coendoM :: (Traversable f, AM m) => CoEndo Id f -> Fix f -> m (Fix f) 
> coendoM = coendoMA

> coendoA :: AnnM a f Identity => CoEndo a f -> FixA a f -> FixA a f 
> coendoA phi = runIdentity . coendoMA phi

> coendo :: Traversable f => CoEndo Id f -> Fix f -> Fix f
> coendo phi = runIdentity . coendoM phi

\end{subsection}

\begin{subsection}{Applicative paramorphisms}

> data Psi (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
>   Alg  :: ((f r, f (FixA a f)) -> r)  -> Psi a f r
>   Prj  :: Psi a f (r -> s, r, s)      -> Psi a f s

> instance Functor f => Functor (Psi a f) where
>   fmap f psi = Prj (pure f <++> psi)

> instance Functor f => Applicative (Psi a f) where
>   pure     = Alg . const
>   a <*> b  = Prj (a <++> b)

% endoMA  :: (Functor m, Lazy m, AnnQ a f m, AnnP a f m)
%         => Endo a f -> FixA a f -> m (FixA a f)
% endoMA psi = endoMA' (return `either` produce) Left psi
% 
% endoMA'  :: (Functor m, Lazy m, AnnQ a f m)
%          => (x -> m r) -> (r -> x) -> Psi a f x -> FixA a f -> m r
% endoMA' z y (Alg psi) f = 
%   do  g   <- query f
%       r   <- fmap' y `fmap` mapM (lazy . endoMA' z y (Alg psi)) g
%       z (psi (r, g))
% endoMA' z y (Prj psi) f = fmap' trd3 (endoMA' f0 f1 psi f)
%     where  f0  (a, b, r) = z r >>= \r' -> return (a, b, r')
%            f1  (a, b, r) = (a, b, y r)
% \end{code}

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

\end{subsection}

\begin{subsection}{Lazy IO and strict paramorphisms}

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
> para' psi = fmap' dseq (fix (\pm -> return . psi <=< mapM (group (lazy . pm)) <=< query))
>   where group f c = fmap ((,) c) (f c)

\end{subsection}

\end{section}

