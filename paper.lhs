\documentclass[10pt,twocolumn,serif,xcolor=dvipsnames]{article}
%\documentclass[12pt,serif,xcolor=dvipsnames]{article}

%include polycode.fmt

\usepackage{amsmath}
\usepackage{pxfonts}
\usepackage{eulervm}
\usepackage[usenames]{color}

\usepackage{draft}

\addtolength{\textheight}{2.4cm}
%\addtolength{\hoffset}{ -0.8cm}
%\addtolength{\voffset}{ -2cm}
\addtolength{\textwidth}{1.0cm}

\definecolor{stress}{rgb}{0.50,0.50,0.50} 
\newcommand{\stress}[1]{\textcolor{stress}{#1}}

\title{Document title}
\author{Sebastiaan Visser}
\date{\today}

\begin{document}
\maketitle
\abstract{abstract here}

\input{intro}

\bibliographystyle{plain}
\bibliography{paper}

\end{document}

