%\documentclass{article}
%\documentclass[12pt]{scrartcl}
%\documentclass[a4wide,10pt]{article}
\documentclass[twoside, a4paper, openright]{report}

%include polycode.fmt

\usepackage{a4wide}
\usepackage[pdftex]{graphicx}
\usepackage[usenames]{color}
\usepackage{amsmath}
\usepackage{draft}
%\usepackage{fancyhdr}
\usepackage{pxfonts}
% \usepackage{mathpazo} % For the fancy font, idea from Martijn/Wouter
\definecolor{linkblue}{RGB}{6,55,206}
\usepackage[colorlinks=true,linkcolor=linkblue]{hyperref} %m must be last \usepackage

\setlength{\parindent}{0pt}
\setlength{\parskip}{0.1in} % \baselineskip}

%\pagestyle{fancy}
%\rhead{\emph{MSc Thesis}}
%\lhead{Generic persistence of Haskell data types}

\bibliographystyle{alpha}

% \addtolength{\voffset}{-0.4in}
% \addtolength{\textheight}{0.6in}
% \addtolength{\hoffset}{-0.5in}
% \addtolength{\textwidth}{1.0in}


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

% \maketitle
% \input{Abstract}
% \pagebreak
% 
% \tableofcontents
% \pagebreak
% 
% \input{Introduction} 
% \input{Motivation} 
% \input{Overview} 
% \input{Fixpoints} 
% \input{Morphisms} 
% \input{Heap} 
% \input{Storage} 
% \input{Example} 
\input{HigherOrder} 
% \input{RelatedWork} 
% \input{Conclusion}
% \input{FutureWork} 
% 
% \pagebreak
% \bibliographystyle{plain}
% \bibliography{thesis}

\end{document}

