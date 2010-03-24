%\documentclass{article}
%\documentclass[12pt]{scrartcl}
\documentclass[a4wide,10pt]{article}

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

\addtolength{\voffset}{-0.4in}
\addtolength{\textheight}{0.6in}
\addtolength{\hoffset}{-0.5in}
\addtolength{\textwidth}{1.0in}


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
<<<<<<< HEAD
\pagebreak

\tableofcontents
\pagebreak

\input{Introduction} 
\input{Motivation} 
\input{Overview} 
\input{Fixpoints} 
\input{Morphisms} 
\input{Heap} 
\input{Storage} 
\input{Example} 
\input{HigherOrder} 
\input{RelatedWork} 
\input{Conclusion}
\input{FutureWork} 
=======

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
>>>>>>> ec9ec14dd49126cbe0c9860b5ea005d1e1fe3194

\pagebreak
\bibliographystyle{plain}
\bibliography{thesis}

\end{document}

