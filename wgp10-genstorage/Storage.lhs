%if False

> {-# LANGUAGE
>     ScopedTypeVariables
>   , FlexibleInstances
>   , FlexibleContexts
>   , MultiParamTypeClasses
>   , UndecidableInstances
>   , GeneralizedNewtypeDeriving
>   #-}
> module Storage where

> import Control.Monad
> import Control.Monad.Trans
> import Control.Applicative
> import Prelude hiding (read, lookup)
> import Data.Traversable
> import Data.Binary
> import Heap
> import Fixpoints
> import Morphisms
> import qualified Generics.Regular.Functions.Binary as G
> import qualified Prelude as Prelude

%endif

\section{Persistent data structures}

\todo{Intro: In section ? we have X and in section ? we have Y and now we combine.}

In the previous section we have deliberately build a |Ptr| type with to type
parameters, the functor type |f| with an explicit index |a|. Due to these two
type parameters the type becomes usable as a fixed point annotation. We can
make the |Ptr| type an instance of both the |Out| and |In| type class from
section \ref{sec:annotations}. We associate the pointer annotation with the |Heap|
type parameters the type becomes usable as a fixed point annotation. We build a
\emph{persistent} binary search tree by using a pointer annotation at the
recursive positions of the tree:

> type TreeP k v = FixA Ptr (TreeF k v)

When we work with a value of type |TreeP k v| we now actually work with a
\emph{pointer} to binary tree somewhere on the heap on disk. The pointer
references a block on the heap that stores a binary serialization of node with
type |TreeF k v (TreeP k v)|; a single node with at the recursive positions
again pointers to the sub structures. In figure \ref{fig:binarytree-pers} we
see our example tree laid out on the heap.

\begin{figure*}[pt]
\begin{center}
\includegraphics[scale=0.3]{img/binarytree-pers.pdf}
\end{center}
\caption{A persistent binary tree that lives on the storage heap. Each node is
stored on its own heap block in binary representation. All sub structures are
referenced by pointer to the file offset.}
\label{fig:binarytree-pers}
\end{figure*}

We make the |Ptr| type an instance of both the |Out| and |In| type class from
section \ref{sec:annotations}. We associate the pointer annotation with the
|Heap| context and use the |read| operations as the implementation for |outA|
and use the |write| operation as the implementation for |inA|:

> instance (Traversable f, Binary (f (FixA Ptr f))) => Out Ptr f Heap
>    where outA = read
>
> instance (Traversable f, Binary (f (FixA Ptr f))) => In Ptr f Heap
>    where inA = write

To make the two instances work we need a |Binary| instance for both the fixed
combinator and the |TreeF| pattern functor. Both instances can be seen in
figure \ref{fig:binary-instances}.

\begin{figure}[pt]
\begin{center}

> instance Binary (f (Fix f)) => Binary (Fix f) where
>   put (In f) = put f
>   get = fmap In get 
>
> instance  (Binary k, Binary v, Binary f)
>       =>  Binary (TreeF k v f) where
>   put Leaf              = do  putWord8 0
>   put (Branch k v l r)  = do  putWord8 1
>                               put k; put v; put l; put r
>   get = do  t <- get
>             if t == (0 :: Word8)
>              then return Leaf
>              else liftM4 Branch get get get get

\end{center}
\caption{The |Binary| instances for the fixed point combinator |Fix| and the
|TreeF| pattern functor.}
\label{fig:binary-instances}
\end{figure}


% We use the example construction function |myTree_a| from section
% \ref{sec:fixann} and specialize the type to the |Ptr| annotation in the |Heap|
% context. We run the function as a heap operations against an empty file.
% 
% \begin{verbatim}
% ghci> let tree = myTree_a :: Heap (TreeP Int Int)
% ghci> run "test.db" tree
% \end{verbatim}





% 
% > instance (Traversable f, Binary (f (FixA Ptr f))) => OutIn Ptr f Heap
% >    where outInA = undefined
% 
% > query :: Binary (f (Fix f)) => (Fix f -> Heap b) -> Heap b
% > query c = read (P 0) >>= c . In
% 
% > produce :: Binary (f (Fix f)) => Heap (Fix f) -> Heap ()
% > produce c = c >>= update (P 0) . out
% 
% > modify :: Binary (f (Fix f)) => (Fix f -> Heap (Fix f)) -> Heap ()
% > modify c = read (P 0) >>= c . In >>= update (P 0) . out
% 
% 
% 
% > fromListP  :: (Ord k, Binary k, Binary v) 
% >            =>  [(k, v)] -> Heap (TreeP k v)
% > fromListP = fromList
% 
% > lookupP :: (Ord k, Binary k, Binary v) => k
% >         -> TreeP k v -> Heap (Maybe v)
% > lookupP = lookup
% 
% > insertP :: (Ord k, Binary k, Binary v) =>
% >      k -> v -> TreeP k v -> Heap (TreeP k v)
% > insertP = insert
% 
% > main :: IO ()
% > main =
% >   do  run "squares.db" $ produce $
% >         (fromListP (map (\a -> (a, a*a)) [1..10::Int]))
% >       putStrLn "Database created."
% 
% > main' :: IO ()
% > main' =
% >   run "squares.db" $ forever $
% >     do liftIO $ putStr "Give a number> "
% >        num <- Prelude.read <$> liftIO getLine
% >        sqr <- query (lookupP num)  -- actual lookup
% >        case sqr of
% >          Nothing -> do modify (insertP num (num * num))
% >                        liftIO (putStrLn "inserted")
% >          Just s  -> liftIO $ print ( num :: Int
% >                                    , s   :: Int
% >                                    )

