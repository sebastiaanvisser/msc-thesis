%\documentclass[10pt,twocolumn,serif,xcolor=dvipsnames]{article}
\documentclass[12pt,serif,xcolor=dvipsnames]{scrartcl}
%\documentclass[serif,xcolor=dvipsnames]{article}

%include polycode.fmt

\usepackage{fancyhdr}
\usepackage{amsmath}
\usepackage{pxfonts}
\usepackage{eulervm}
\usepackage[usenames]{color}

\usepackage{draft}

\pagestyle{fancy}

\rhead{\emph{proposal}}
\lhead{Generic persistence of Haskell data types}
\renewcommand{\headrulewidth}{0.4pt}

\addtolength{\textheight}{0.6in}
%\addtolength{\voffset}{ -0.6in}
%\addtolength{\textwidth}{0.6in}
%\addtolength{\hoffset}{ -0.3in}

%\setlength{\parindent}{0in}
%\setlength{\parskip}{\baselineskip}
% \setlength{\columnsep}{0.4in}

% \setlength{\columnseprule}{0.001in}
% \addtolength{\evensidemargin}{ -0.2in}
% \addtolength{\oddsidemargin}{ -0.2in}
% \setlength{\itemindent}{0in}
% \setlength{\itemsep}{0in}

\definecolor{stress}{rgb}{0.50,0.50,0.50} 
\newcommand{\stress}[1]{\textcolor{stress}{#1}}

\title{\emph{proposal:}\\Generic persistence of\\ Haskell data types}
\author{Sebastiaan Visser}
\date{\today}

% -----------------------------------------------------------------------------

\begin{document}

\maketitle
\input{abstract}       \pagebreak
\tableofcontents       \pagebreak

\label{intro}        \input{intro}        % \pagebreak
\label{framework}    \input{framework}    % \pagebreak
\label{motivation}   \input{motivation}   % \pagebreak
\label{goal}         \input{goal}         % \pagebreak
\label{planning}     \input{planning}     % \pagebreak
\label{extras}       \input{extras}       % \pagebreak
\label{related-work} \input{related-work}   \pagebreak

\bibliographystyle{plain}
\bibliography{proposal}

\end{document}

