\documentclass[10pt,twocolumn,serif,xcolor=dvipsnames]{article}

%include polycode.fmt

\usepackage{amsmath}
\usepackage{pxfonts}
\usepackage{eulervm}
%\usepackage{mathpazo}
\usepackage[usenames]{color}

% \usepackage{my}
\usepackage{draft}

\setlength{\hoffset}{ -1in}
\setlength{\voffset}{ -1in}

\setlength{\topmargin}{1in}
\setlength{\headheight}{0pt}
\setlength{\headsep}{0pt}

\setlength{\evensidemargin}{.75in}
\setlength{\oddsidemargin}{.75in}

% \addtolength{\textheight}{1.2in}
% \addtolength{\voffset}{ -0.6in}
% \addtolength{\textwidth}{1.0in}
% \addtolength{\hoffset}{ -0.5in}
% \setlength{\parindent}{0pt}
% \setlength{\parskip}{\baselineskip}

\definecolor{stress}{rgb}{0.50,0.50,0.50} 
\newcommand{\stress}[1]{\textcolor{stress}{#1}}

\title{Proposal draft: Generic persistency of Haskell data types\\
\large{Tying the knot, persistently.}}
\authorinfo{Sebastiaan Visser}
\date{\today}

\authorinfo{Sebastiaan Visser}
           {University Utrecht}
           {sfvisser@cs.uu.nl}

\begin{document}
\maketitle
\abstract{\review{
Algebraic data types (ADTs) are a powerful way to structure data in Haskell,
functions operating on these ADTs can be used to process the data.  When
dealing with large data sets that do not fit into the computer memory at once,
tricks have to be used to persist data on external storage devices.  There are
several techniques for data persistency available for Haskell, unfortunately,
none of them transparent to the user.  This document proposes a new persistency
framework for Haskell that is transparent to the user, does not rely on
existing database systems, uses generic programming to avoid to much
boilerplate and does not compromise the functional paradigm.}}

\input{intro}
\input{cont}
\input{stor}
\input{seri}
\input{pers}
\input{more}

\bibliographystyle{plain}
\bibliography{paper}

\end{document}

