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
context and use the |read| operations as the implementation for |outA| and use
the |write| operation as the implementation for |inA|:

> instance (Traversable f, Binary (f (FixA Ptr f))) => Out Ptr f Heap
>    where outA = read
>
> instance (Traversable f, Binary (f (FixA Ptr f))) => In Ptr f Heap
>    where inA = write

> type TreeP k v = FixA Ptr (TreeF k v)






% instance Binary (f (Fix f)) => Binary (Fix f) where
%   put (In f) = put f
%   get = In <$> get 

% > instance (Binary k, Binary v, Binary f) => Binary (TreeF k v f) where
% >   put = G.gput
% >   get = G.gget
% 
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
% \begin{figure*}[pt]
% \label{fig:binarytree}
% \begin{center}
% \includegraphics[scale=0.3]{img/binarytree-pers.pdf}
% \end{center}
% \caption{An example of a binary tree.}
% \end{figure*}
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

