%include polycode.fmt
%include thesis.fmt

%if False

\begin{code}
{-# LANGUAGE KindSignatures, UndecidableInstances #-}
module Morphisms where

import Control.Applicative
import Control.Category
import Control.Monad.Identity
import Control.Monad.Reader hiding (mapM)
import System.IO.Unsafe
import Data.Traversable
import Prelude hiding ((.), id, mapM)
import Fixpoints
\end{code}

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

\begin{code}
type Psi1 a f r = f (FixA a f :*: r) -> r
\end{code}

\noindent
This type signature describes that an algebra should be able to produce an
value of type |r| from one non-recursive pieces of a recursive structure, all
the recursive sub-results of the computation and the original sub-structures.

An example of such an algebra is the |lookupAlg| function for binary trees.
Because the algebra only uses the recursive sub-results and not the origianl
sub-structures this algebra is actually a catamorphism, a special case of the
more general paramorpism.

\begin{code}
lookupAlg :: Ord v => v -> Psi1 a (Tree_f v) (Maybe v)
lookupAlg _  Leaf_2                      = Nothing
lookupAlg v  (Branch_2 c (_, l) (_, r))  = 
  case v `compare` c of
    LT  -> l
    EQ  -> Just c
    GT  -> r
\end{code}

\noindent
The paramorphism function performs a bottom up traversal over some |Functor|
and for every non-recursive piece applies the algebra. The most generic version
of this paramorphism within our annotation framework is the |paraMA1|. This
function runs is some monadic context |m| and performs a traversal over some
annotated structure |FixA a f| using the |AnnQ| type class to perform
annotation specific computations, hence the $(_{\alpha}^m)$ postfixes.

\begin{code}
paraMA1 :: AnnQ a f m => Psi1 a f r -> FixA a f -> m r
paraMA1 psi = return . psi <=< mapM (group (paraMA1 psi)) <=< query
  where group f c = fmap ((,) c) (f c)
\end{code}

\noindent
The implementation of this generic paramorphism might seem a bit cryptic at
first sight, this is probably due to its very generic behaviour. Quickly
summarized this function performs a bottom up traversal over a recursively
annotation structure. This functions gets a fully annotated structure as input
and uses the |query| function to get the original structure out of the
annotation. The |Traversable| instance that is an implicit super class of the
|AnnQ| class allows us to use the |mapM| function to recursively apply this
|paraMA1| function to the sub-structures to come up with the sub-results.
The sub-results will be grouped together with the original sub-structures these
results are computed from. The original non-recursive piece of the input with
these grouped results in as the values will be passed into the algebra |psi|.
The algebra can now compute the result value for one level of the recursive
computation. 

To illustrate the usage of the |paraMA1| function we apply this paramorphism to
the |lookup| algebra and get back a function that performs a lookup over an
annotation binary tree.

\begin{code}
lookupMA :: (Ord v, AnnQ a (Tree_f v) m) => v -> FixA a (Tree_f v) -> m (Maybe v)
lookupMA v = paraMA1 (lookupAlg v)
\end{code}

\noindent
When an annotation does not have any requirements about the type of context to
run in we can use the |Identity| monad to create a pure paramorphic traversal.

\begin{code}
paraA1 :: (AnnQ a f Identity, Traversable f) => Psi1 a f c -> FixA a f -> c
paraA1 psi = runIdentity . paraMA1 psi
\end{code}

\noindent
When we further restrict the annotation to be the identity annotation we get
back a true pure paramorphism functions that works on plain in-memory data
structures.

\begin{code}
para1 :: Traversable f => Psi1 Id f c -> Fix f -> c
para1 psi = paraA1 psi
\end{code}

\noindent
To illustrate this pure paramorphism we apply it to the |lookup| algebra and
get back a pure |lookup| function.

\begin{code}
type Tree v = Fix (Tree_f v)
\end{code}

\begin{code}
lookup :: Ord v => v -> Tree v -> Maybe v
lookup v = para1 (lookupAlg v)
\end{code}

\end{subsection}

\begin{subsection}{Apomorphisms}

Dual to the paramorphism is the apomorphisms. Where the paramorphism abstract
away from recursion, the apomorphisms abstracts away from corecursion.
Similarly, apomorphisms use coalgebras to describe corecursive operations.
Apomorphisms are generalizations of anamorphisms, the |unfold| function that
can be used to create lists from a seed value is an example of an anamorphisms.

The coalgebra for an apomorphism, called |Phi|, takes a seed value of some type
|s| and should be able to produce an new seed or a recursive structure.
  
\begin{code}
type Phi a f s = s -> f (s :+: f (FixA a f))
\end{code}

\noindent
From the type signature of the |Phi| coalgebra it is obvious that it is dual to
the |Psi| algebra for paramorphisms. Paramorphisms destruct recursive
structures to a result value |r|, apomorphisms constructor recursive structures
from a seed value |s|.

\begin{code}
apoMA :: (Functor m, Traversable f, AnnP a f m) => Phi a f s -> s -> m (FixA a f)
apoMA phi = produce <=< mapM (apoMA phi `either` produce) . phi
\end{code}

\begin{code}
apoM :: (Functor m, Traversable f, AnnP Id f m) => Phi Id f s -> s -> m (Fix f)
apoM = apoMA
\end{code}

\begin{code}
apoA :: (Traversable f, AnnP a f Identity) => Phi a f s -> s -> FixA a f
apoA phi = runIdentity . apoMA phi
\end{code}

\begin{code}
apo :: Traversable f => Phi Id f s -> s -> Fix f
apo phi = runIdentity . apoM phi
\end{code}

\end{subsection}

\begin{subsection}{Endomorphic paramorphism}

\begin{code}
type Endo a f = Psi a f (FixA a f :+: f (FixA a f))
\end{code}

\begin{code}
toEndo :: Functor f => Psi a f (FixA a f) -> Endo a f
toEndo = fmap' Left
\end{code}

\begin{code}
endoMA  :: (Functor m, Lazy m, AnnQ a f m, AnnP a f m)
        => Endo a f -> FixA a f -> m (FixA a f)
endoMA psi = endoMA' (return `either` produce) Left psi
\end{code}

\begin{code}
endoMA'  :: (Functor m, Lazy m, AnnQ a f m)
         => (x -> m r) -> (r -> x) -> Psi a f x -> FixA a f -> m r
endoMA' z y (Alg psi) f = 
  do  g   <- query f
      r   <- fmap' y `fmap` mapM (lazy . endoMA' z y (Alg psi)) g
      z (psi (r, g))
endoMA' z y (Prj psi) f = fmap' trd3 (endoMA' f0 f1 psi f)
    where  f0  (a, b, r) = z r >>= \r' -> return (a, b, r')
           f1  (a, b, r) = (a, b, y r)
\end{code}

\begin{code}
endoM :: (Traversable f, Lazy m, AM m) => Endo Id f -> Fix f -> m (Fix f)
endoM psi = endoMA psi
\end{code}

\begin{code}
endoA :: (AnnQ a f Identity, AnnP a f Identity) => Endo a f -> FixA a f -> FixA a f
endoA psi = runIdentity . endoMA psi
\end{code}

\begin{code}
endo :: Traversable f => Endo Id f -> Fix f -> Fix f
endo psi = runIdentity . endoM psi
\end{code}

\end{subsection}

\begin{subsection}{Endomorphic apomorphisms}

\begin{code}
type CoEndo a f = f (FixA a f) -> f (FixA a f :+: (FixA a f :+: f (FixA a f)))
\end{code}

\begin{code}
coendoMA :: (Traversable f, AnnM a f m) => CoEndo a f -> FixA a f -> m (FixA a f)
coendoMA phi = modify (mapM cont . phi)
  where
  cont (Left x)           = coendoMA phi x
  cont (Right (Left  x))  = return x
  cont (Right (Right x))  = produce x
\end{code}

\begin{code}
coendoM :: (Traversable f, AM m) => CoEndo Id f -> Fix f -> m (Fix f) 
coendoM = coendoMA
\end{code}

\begin{code}
coendoA :: (AnnM a f Identity) => CoEndo a f -> FixA a f -> FixA a f 
coendoA phi = runIdentity . coendoMA phi
\end{code}

\begin{code}
coendo :: Traversable f => CoEndo Id f -> Fix f -> Fix f
coendo phi = runIdentity . coendoM phi
\end{code}

\end{subsection}

\begin{subsection}{Applicative paramorphisms}

\begin{code}
data Psi (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Alg  :: ((f r, f (FixA a f)) -> r)  -> Psi a f r
  Prj  :: Psi a f (r -> s, r, s)      -> Psi a f s
\end{code}

\begin{code}
instance Functor f => Functor (Psi a f) where
  fmap f psi = Prj (pure f <++> psi)
\end{code}

\begin{code}
instance Functor f => Applicative (Psi a f) where
  pure     = Alg . const
  a <*> b  = Prj (a <++> b)
\end{code}

%if False

\begin{code}
idPsi :: Functor f => Psi a f (r -> r)
idPsi = pure id
\end{code}

\begin{code}
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
\end{code}

\begin{code}
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
\end{code}

\begin{code}
trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
\end{code}

%endif

\begin{code}
(<++>)  :: (Functor f, Functor (Psi a f)) => Psi a f (r -> s) -> Psi a f r -> Psi a f (r -> s, r, s)
Alg  f <++> Prj  g = Prj (idPsi <++> Alg f) <++> Prj g
Prj  f <++> Alg  g = Prj f <++> Prj (idPsi <++> Alg g)
Prj  f <++> Prj  g = fmap' trd3 f <++> fmap' trd3 g 
Alg  f <++> Alg  g = Alg (\(a, b) -> f (fmap' fst3 a, b) `mk` g (fmap' snd3 a, b))
  where mk x y = (x, y, x y)
\end{code}

\begin{code}
paraMA :: (Traversable f, Lazy m, Functor m, AnnQ a f m) => Psi a f r -> FixA a f -> m r
paraMA (Prj psi) f = fmap' trd3 (paraMA psi f)
paraMA (Alg psi) f = elipses
\end{code}

%if False

\begin{code}
  where elipses =
          do g <- query f
             r <- mapM (lazy . paraMA (Alg psi)) g
             return (psi (r, g))
\end{code}

%endif

\end{subsection}

\begin{subsection}{Lazy IO and strict paramorphisms}

\begin{code}
class Lazy m where
  lazy :: m a -> m a
\end{code}

\begin{code}
instance Lazy Identity where
  lazy = id
\end{code}

\begin{code}
instance (AM m, Lazy m) => Lazy (ReaderT r m) where
  lazy c = ask >>= lift . lazy . runReaderT c
\end{code}

\begin{code}
instance Lazy IO where
  lazy = unsafeInterleaveIO
\end{code}

\begin{code}
dseq :: a -> a
\end{code}

%if False

\begin{code}
dseq = undefined
\end{code}

%endif

\begin{code}
para' :: (Lazy m, AnnQ a f m) => Psi1 a f r -> FixA a f -> m r
para' psi = fmap' dseq (fix (\pm -> return . psi <=< mapM (group (lazy . pm)) <=< query))
  where group f c = fmap ((,) c) (f c)
\end{code}

\end{subsection}

\end{section}

