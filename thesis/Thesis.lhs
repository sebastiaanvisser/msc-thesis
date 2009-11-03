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

> module Thesis where

> import Morphisms
> import Example
> import Storage

%endif

\tableofcontents \pagebreak
\input{Morphisms} 
\input{Example} 
\input{Storage} 

\end{document}

