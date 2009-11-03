\documentclass{article}

%include polycode.fmt
\usepackage{amsmath}
\usepackage{pxfonts}

\title{notitle}
\author{Sebastiaan Visser}
\date{\today}

% -----------------------------------------------------------------------------

\begin{document}

%if False

> {-# LANGUAGE KindSignatures #-}

> import Control.Applicative
> import Control.Category
> import Control.Monad.Identity
> import Control.Monad.Reader hiding (sequence)
> import System.IO.Unsafe
> import Data.Foldable
> import Data.Monoid hiding (Endo)
> import Data.Traversable
> import Data.Traversable
> import Prelude hiding ((.), id, sequence)

%endif

\tableofcontents \pagebreak

%format mempty    = "\varnothing"
%format `mappend` = "\diamond"
%format fmap' a   = "\widehat{" a "}"
%format <*>       = "\circledast"
%format <=<       = "\triangleleft"
%format >>=       = "\rightarrowtail"

%format :*: = "\times"
%format :+: = "+"

%if False

> fmap' :: Functor f => (a -> b) -> f a -> f b
> fmap' = fmap

> infixl 6 :+:
> infixl 7 :*:

> type a :+: b = Either a b
> type a :*: b = (a, b)

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

> data Tree_1 k = Leaf | Branch_1 k (Tree_1 k) (Tree_1 k)

> data Tree_f k f = Leaf_2 | Branch_2 k f f

> newtype Fix_1 (f :: * -> *) = In_1 { out_1 :: f (Fix_1 f) }

> type Tree_2 k = Fix (Tree_f k)

> instance Functor (Tree_f k) where
>   fmap _  Leaf_2            = Leaf_2
>   fmap f  (Branch_2 k l r)  = Branch_2 k (f l) (f r)

> instance Foldable (Tree_f k) where
>   foldMap _  Leaf_2            = mempty
>   foldMap f  (Branch_2 _ l r)  = f l `mappend` f r

> instance Traversable (Tree_f k) where
>   traverse _  Leaf_2            = pure Leaf_2
>   traverse f  (Branch_2 k l r)  = pure (Branch_2 k) <*> f l <*> f r

\subsection{Annotations}

%format FixT  = "\mu_T"
%format FixT1 = "\mu_T^1"
%format Fix   = "\mu"
%format Fix1  = "\mu^1"

> newtype FixT (a :: (* -> *) -> (* -> *)) f = In { out :: a f (FixT a f) }

> type FixT1 a f = a f (FixT a f)

> newtype Id f a = Id { unId :: f a }

> type Fix f = FixT Id f

> type Fix1 f = f (Fix f)

\subsection{Annotation associated functionality}

%format AnnQ = "Ann_Q"
%format AnnP = "Ann_P"

> type Produce a f m = f (FixT a f) -> m (FixT1 a f)

> type Query a f m = FixT1 a f -> m (f (FixT a f))

> type Modify a f m =  ((   f (FixT a f)) -> m  (   f (FixT a f)))
>                  ->  ((a  f (FixT a f)) -> m  (a  f (FixT a f)))

We add the |Traversable| and |Monad| class constraints only to prune the
contexts when using these classes in our functions later.

> class (Traversable f, Monad m) => AnnQ a f m where
>   query :: Query a f m

> class (Traversable f, Monad m) => AnnP a f m where
>   produce :: Produce a f m

> class (Applicative m, Monad m, AnnQ a f m, AnnP a f m) => AnnM a f m where
>   modify :: Modify a f m
>   modify f = produce <=< f <=< query

> instance (Traversable f, Monad m) => AnnQ Id f m where
>   query = return . unId

> instance (Traversable f, Monad m) => AnnP Id f m where
>   produce = return . Id

> instance (Traversable f, Applicative m, Monad m) => AnnM Id f m

\subsection{Paramorphisms}

%format paraMT1 = "para^m_T"
%format paraM1  = "para^m"
%format paraT1  = "para_T"
%format para1   = "para"
%format psi     = "\psi"
%format Psi1    = "\Psi"
%format Psi     = "\Psi"

> type Psi1 a f r = f r :*: f (FixT a f) -> r

> paraMT1 :: AnnQ a f m => Psi1 a f r -> FixT1 a f -> m r
> paraMT1 psi f = 
>   do   g  <- query f
>        r  <- sequence (fmap (paraMT1 psi . out) g)
>        return (psi (r, g))

> paraM1 :: (Monad m, Traversable f) => Psi1 Id f r -> Fix f -> m r
> paraM1 psi = paraMT1 psi . out

> paraT1 :: (AnnQ a f Identity, Traversable f) => Psi1 a f c -> FixT a f -> c
> paraT1 psi = runIdentity . paraMT1 psi . out

> para1 :: Traversable f => Psi1 Id f c -> Fix f -> c
> para1 psi = runIdentity . paraM1 psi

\subsection{Apomorphisms}

%format apoMT = "apo^m_T"
%format apoM  = "apo^m"
%format apoT  = "apo_T"
%format apo   = "apo"
%format phi   = "\phi"
%format Phi   = "\Phi"

> type Phi a f s = s -> f (s :+: f (FixT a f))

> apoMT :: (Functor m, Traversable f, AnnP a f m) => Phi a f s -> s -> m (FixT a f)
> apoMT phi = fmap' In . produce <=< sequence . fmap (apoMT phi `either` (fmap' In . produce)) . phi

> apoM :: (Functor m, Traversable f, AnnP Id f m) => Phi Id f s -> s -> m (Fix f)
> apoM = apoMT

> apoT :: (Traversable f, AnnP a f Identity) => Phi a f s -> s -> FixT a f
> apoT phi = runIdentity . apoMT phi

> apo :: Traversable f => Phi Id f s -> s -> Fix f
> apo phi = runIdentity . apoM phi

\subsection{Lazy IO and strict paramorphisms}

> class Lazy m where
>   lazy :: m a -> m a

> instance Lazy Identity where
>   lazy = id

> instance (Monad m, Lazy m) => Lazy (ReaderT r m) where
>   lazy c = ask >>= lift . lazy . runReaderT c

> instance Lazy IO where
>   lazy = unsafeInterleaveIO

> dseq :: a -> a

%if False

> dseq = undefined

%endif

> para' :: (Lazy m, AnnQ a f m) => Psi1 a f r -> FixT1 a f -> m r
> para' psi = fmap' dseq p
>   where
>   p f = 
>     do   g <- query f
>          r <- sequence (fmap (lazy . p . out) g)
>          return (psi (r, g))

\subsection{Applicative paramorphisms}

%format <++> = "\oplus"
%format paraMT = "para^m_T"
%format elipses = "\dots"

> data Psi (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
>   Alg  :: ((f r, f (FixT a f)) -> r)  -> Psi a f r
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

> paraMT :: (Traversable f, Lazy m, Functor m, AnnQ a f m) => Psi a f r -> FixT1 a f -> m r
> paraMT (Prj psi) f = fmap' trd3 (paraMT psi f)
> paraMT (Alg psi) f = elipses

%if False

>   where elipses =
>           do g <- query f
>              r <- sequence (fmap (lazy . paraMT (Alg psi) . out) g)
>              return (psi (r, g))

%endif

\subsection{Endomorphic paramorphism}

%format Endo    = "\Psi_{endo}"
%format endoMT  = " endo^m_T"
%format endoMT' = "helper"
%format endoM   = "endo^m"
%format endoT   = "endo_T"
%format endo    = "endo"

> type Endo a f = Psi a f (FixT a f :+: f (FixT a f))

> toEndo :: Functor f => Psi a f (FixT a f) -> Endo a f
> toEndo = fmap' Left

> endoMT  :: (Functor m, Lazy m, AnnQ a f m, AnnP a f m)
>         => Endo a f -> FixT a f -> m (FixT a f)
> endoMT psi = fmap' In . endoMT' ((return . out) `either` produce) (Left . In) psi . out

> endoMT'  :: (Functor m, Lazy m, AnnQ a f m)
>          => (x -> m r) -> (r -> x) -> Psi a f x -> FixT1 a f -> m r
> endoMT' z y (Alg psi) f = 
>   do  g   <- query f
>       r   <- fmap' y `fmap` sequence (fmap (lazy . endoMT' z y (Alg psi) . out) g)
>       z (psi (r, g))
> endoMT' z y (Prj psi) f = fmap' trd3 (endoMT' f0 f1 psi f)
>     where  f0  (a, b, r) = z r >>= \r' -> return (a, b, r')
>            f1  (a, b, r) = (a, b, y r)

> endoM :: (Traversable f, Lazy m, Applicative m, Monad m) => Endo Id f -> Fix f -> m (Fix f)
> endoM psi = endoMT psi

> endoT :: (AnnQ a f Identity, AnnP a f Identity) => Endo a f -> FixT a f -> FixT a f
> endoT psi = runIdentity . endoMT psi

> endo :: Traversable f => Endo Id f -> Fix f -> Fix f
> endo psi = runIdentity . endoM psi

\subsection{Endomorphic apomorphisms}

%format CoEndo    = "\Phi_{endo}"
%format coendoMT  = " coendo^m_T"
%format coendoM   = "coendo^m"
%format coendoT   = "coendo_T"
%format coendo    = "coendo"

> type CoEndo a f = f (FixT a f) -> f (FixT a f :+: (FixT a f :+: f (FixT a f)))

> coendoMT :: (Traversable f, AnnM a f m) => CoEndo a f -> FixT a f -> m (FixT a f)
> coendoMT phi = fmap In . (modify (sequence . fmap cont . phi)) . out 
>   where
>   cont (Left x)           = coendoMT phi x
>   cont (Right (Left  x))  = return x
>   cont (Right (Right x))  = fmap' In (produce x)

> coendoM :: (Traversable f, Applicative m, Monad m) => CoEndo Id f -> Fix f -> m (Fix f) 
> coendoM = coendoMT

> coendoT :: (AnnM a f Identity) => CoEndo a f -> FixT a f -> FixT a f 
> coendoT phi = runIdentity . coendoMT phi

> coendo :: Traversable f => CoEndo Id f -> Fix f -> Fix f
> coendo phi = runIdentity . coendoM phi

\section{Binary tree example}

\subsection{Debug annotation}

\subsection{Size and lookup}

\subsection{Repmin}

\subsection{Insertion and deletion}

\section{Data storage}

\subsection{Heap}

\subsubsection{Heap layout}
\subsubsection{Reading}
\subsubsection{Allocation}
\subsubsection{Writing}

\subsection{Storage annotation}

\subsection{Persistent map}

\end{document}

