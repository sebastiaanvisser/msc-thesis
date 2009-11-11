\documentclass{article}

%include polycode.fmt
\usepackage{amsmath}
\usepackage{pxfonts}

\title{notitle}
\author{Sebastiaan Visser}
\date{\today}

%if False

> module Thesis where
> import Morphisms ()
> import Example ()
> import Storage ()

%endif

\begin{document}
  \tableofcontents
  \pagebreak
  \input{Introduction} 
  \pagebreak
  \input{Fixpoints} 
  \pagebreak
  \input{Morphisms} 
  \pagebreak
  \input{Example} 
  \input{Storage} 
\end{document}

