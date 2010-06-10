%if False

> {-# LANGUAGE
>     ScopedTypeVariables
>   , FlexibleInstances
>   , FlexibleContexts
>   , MultiParamTypeClasses
>   , UndecidableInstances
>   , GeneralizedNewtypeDeriving
>   #-}
> module Appendix where

> import Control.Monad
> import Control.Monad.Trans
> import Heap
> import Prelude hiding (read, lookup)
> import Storage
> import qualified Prelude as Prelude

%endif

\appendix
\section{Example program}

This is \todo{what is this}

> main :: IO ()
> main =
>   run "squares.db" $
>     do  produce squaresP
>         liftIO . putStrLn $
>           "Created initial squares database."
>         forever $
>           do  liftIO (putStr "Give a number> ")
>               num <- liftM Prelude.read (liftIO getLine)
>               sqr <- query (lookupP num)
>               case sqr of
>                 Just s   -> (liftIO . print) (num :: Int, s :: Int)
>                 Nothing  -> do  modify (insertP num (num * num))
>                                 liftIO (putStrLn "inserted")

\acks

Acknowledgments, if needed.

% We recommend abbrvnat bibliography style.
% 
% \bibliographystyle{abbrvnat}
% 
% The bibliography should be embedded for final submission.
% 
% \begin{thebibliography}{}
% \softraggedright
% 
% \bibitem[Smith et~al.(2009)Smith, Jones]{smith02}
% P. Q. Smith, and X. Y. Jones. ...reference text...
% 
% \end{thebibliography}

