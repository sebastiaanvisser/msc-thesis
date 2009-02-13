\documentclass[10pt,twocolumn,serif,xcolor=dvipsnames]{article}

%include polycode.fmt

\usepackage{fancyhdr}
\usepackage{amsmath}
\usepackage{pxfonts}
\usepackage{eulervm}
\usepackage[usenames]{color}

\usepackage{draft}

\pagestyle{fancy}

\rhead{\emph{PRE-PROPOSAL}}
\lhead{Generic persistency of Haskell data types}
\renewcommand{\headrulewidth}{0.4pt}

\addtolength{\textheight}{1.2in}
\addtolength{\voffset}{ -0.6in}
\addtolength{\textwidth}{1.0in}
\addtolength{\hoffset}{ -0.5in}
\setlength{\parindent}{0in}
\setlength{\parskip}{\baselineskip}
\setlength{\columnsep}{0.4in}

%\setlength{\columnseprule}{0.001in}
%\addtolength{\evensidemargin}{ -0.2in}
%\addtolength{\oddsidemargin}{ -0.2in}
%\setlength{\itemindent}{0in}
%\setlength{\itemsep}{0in}

\definecolor{stress}{rgb}{0.50,0.50,0.50} 
\newcommand{\stress}[1]{\textcolor{stress}{#1}}

\title{PRE-PROPOSAL: Generic persistency of Haskell data types\\
\large{Tying the knot, persistently.}}
\author{Sebastiaan Visser}
\date{\today}

\begin{document}
\maketitle

\abstract{Algebraic data types (ADTs) are a powerful way to structure data in
Haskell, functions operating on these ADTs can be used to process the data.
When dealing with large data sets that do not fit into computer memory at once,
tricks have to be used to persist data on external storage devices.  There are
several techniques for data persistency available for Haskell, unfortunately,
none of them truly transparent to the user.  This document proposes a new
persistency framework for Haskell that uses purely functional data structure to
manage data stored on disk. No external database tools are used so the system
remains lightweight and does not compromise the functional paradigm.}

\input{intro}
\input{goal}
\input{cont}
\input{stor}
\input{seri}
\input{pers}
\input{more}

\bibliographystyle{plain}
\bibliography{pre-proposal}

\end{document}

