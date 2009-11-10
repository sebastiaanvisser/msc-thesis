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
To generalize this pattern we abstract away from recursion when writing the
structure-processing algorithms using morphisms. We start out with the
\emph{paramorphism}, which is a generalization of the \emph{catamorphism}, a
bottom up traversal that can fold an entire structure into a single value.

We first write down the type signature of the algebra for the paramorphism, we
call this algebra |Psi1|. 

> type Psi1 a f r = f (FixA a f :*: r) -> r

This type signature describes that an algebra should be able to produce an
value of type |r| from one non-recursive pieces of a recursive structure, all
the recursive sub-results of the computation and the original sub-structures.

An example of such an algebra is the |lookup| function for binary trees.
Because the algebra only uses the recursive sub-results and not the origianl
sub-structures this algebra is actually a catamorphism, a special case of the
more general paramorpism.

> lookup :: Ord v => v -> Psi1 a (Tree_f v) (Maybe v)
> lookup _  Leaf_2                      = Nothing
> lookup v  (Branch_2 c (_, l) (_, r))  = 
>   case v `compare` c of
>     LT  -> l
>     EQ  -> Just c
>     GT  -> r

The paramorphism function performs a bottom up traversal over some |Functor|
and for every non-recursive piece applies the algebra. The most generic version
of this paramorphism within our annotation framework is the |paraMA1|. This
function runs is some monadic context |m| and performs a traversal over some
annotated structure |FixA a f| using the |AnnQ| type class to perform
annotation specific computations, hence the $(_{\alpha}^m)$ postfixes.

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

