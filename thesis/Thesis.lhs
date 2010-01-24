\documentclass{article}

%include polycode.fmt
\usepackage{amsmath}
\usepackage{pxfonts}
% \usepackage[charter]{mathdesign}
\usepackage[usenames]{color}
\usepackage{draft}

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

  \maketitle
  \input{Abstract}

  \pagebreak \tableofcontents
  \pagebreak \input{Introduction} 
  \pagebreak \input{Motivation} 
  \pagebreak \input{Fixpoints} 
  \pagebreak \input{Morphisms} 
  \pagebreak \input{Example}
             \input{Storage} 
  \pagebreak \input{RelatedWork} 
  \pagebreak \input{FutureWork} 

  \pagebreak
  \bibliographystyle{plain}
  \bibliography{thesis}

\end{document}

