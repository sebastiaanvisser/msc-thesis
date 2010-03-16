%\documentclass{article}
%\documentclass[12pt]{scrartcl}
\documentclass[a4wide,11pt]{article}

%include polycode.fmt

\usepackage[pdftex]{graphicx}
\usepackage[usenames]{color}
\usepackage{amsmath}
\usepackage{draft}
%\usepackage{fancyhdr}
\usepackage{pxfonts}

%\pagestyle{fancy}
%\rhead{\emph{MSc Thesis}}
%\lhead{Generic persistence of Haskell data types}

%\addtolength{\voffset}{-0.4in}
%\addtolength{\textwidth}{0.8in}
%\addtolength{\hoffset}{-0.4in}


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
\pagebreak \input{Overview} 
\pagebreak \input{Fixpoints} 
\pagebreak \input{Morphisms} 
\pagebreak \input{Heap} 
\pagebreak \input{Storage} 
\pagebreak \input{Example} 
\pagebreak \input{HigherOrder} 
\pagebreak \input{FingerTree} 
\pagebreak \input{RelatedWork} 
\pagebreak \input{Conclusion}
\pagebreak \input{FutureWork} 

\pagebreak
\bibliographystyle{plain}
\bibliography{thesis}

\end{document}

