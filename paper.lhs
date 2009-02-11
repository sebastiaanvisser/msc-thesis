\documentclass[10pt,twocolumn,serif,xcolor=dvipsnames]{article}

%include polycode.fmt

\usepackage{amsmath}
\usepackage{pxfonts}
\usepackage{eulervm}
\usepackage[usenames]{color}

\usepackage{draft}

\addtolength{\textheight}{1in}
\addtolength{\voffset}{ -0.5in}
\addtolength{\textwidth}{0.4in}
\addtolength{\hoffset}{ -0.2in}
%\setlength{\parindent}{0pt}
%\setlength{\parskip}{0.06in} % \baselineskip}

\definecolor{stress}{rgb}{0.50,0.50,0.50} 
\newcommand{\stress}[1]{\textcolor{stress}{#1}}

\title{Proposal draft: Generic persistency of Haskell data types}
\author{Sebastiaan Visser}
\date{\today}

\begin{document}
\maketitle
\abstract{abstract here}

\input{intro}

\bibliographystyle{plain}
\bibliography{paper}

\end{document}

