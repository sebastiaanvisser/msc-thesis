%include polycode.fmt
%include thesis.fmt

%if False

> {-# LANGUAGE
>     KindSignatures
>   , GeneralizedNewtypeDeriving
>   , FlexibleContexts
>   , UndecidableInstances
>   , FlexibleInstances
>   , MultiParamTypeClasses
>   #-}
> module Storage where

> import Data.Traversable
> import Control.Applicative
> import Data.Binary
> import Fixpoints
> import Heap
> import Prelude hiding (read)

%endif

\section{Generic storage}

In this section we will describe how to use the generic annotation framework
for recursive datatypes together with the file based storage heap to build a
generic storage framework.

\subsection{Pointer annotation}

In the previous section about the storage heap we have seen 



> instance  (Traversable f, Binary (f (FixA Pointer f)))
>       =>  AnnQ Pointer f HeapR where
>   query = read . out

> instance  (Traversable f, Binary (f (FixA Pointer f)))
>       =>  AnnQ Pointer f HeapW where
>   query = liftR . read . out

> instance  (Traversable f, Binary (f (FixA Pointer f)))
>       =>  AnnP Pointer f HeapW where
>   produce = fmap In . write

> instance  (Traversable f, Binary (f (FixA Pointer f)))
>       =>  AnnM Pointer f HeapW where

> instance Binary (a f (FixA a f)) => Binary (FixA a f) where
>   put = put . out
>   get = In <$> get

