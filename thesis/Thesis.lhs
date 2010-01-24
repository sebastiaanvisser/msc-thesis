%\documentclass{article}
%\documentclass[12pt]{scrartcl}
\documentclass[a4paper,12pt]{article}

%include polycode.fmt

\usepackage{fancyhdr}
\usepackage{amsmath}
\usepackage{pxfonts}
\usepackage[usenames]{color}
\usepackage{draft}


\pagestyle{fancy}
\rhead{\emph{MSc Thesis}}
%\lhead{Generic persistence of Haskell data types}

%\addtolength{\voffset}{0.4in}
%\addtolength{\textwidth}{-0.8in}
%\addtolength{\hoffset}{0.4in}


%if False

> module Thesis where
> import Morphisms ()
> import Example ()
> import Storage ()

%endif

\title{A GENERIC APPROACH TO DATATYPE PERSISTENCY IN HASKELL}
\author{Sebastiaan Visser}
\date{\today}

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

