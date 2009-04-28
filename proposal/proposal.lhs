% \documentclass[10pt,twocolumn,serif,xcolor=dvipsnames]{article}
\documentclass[12pt,serif,xcolor=dvipsnames]{article}

%include polycode.fmt

% \usepackage{fancyhdr}
\usepackage{amsmath}
\usepackage{pxfonts}
\usepackage{eulervm}
\usepackage[usenames]{color}

\usepackage{draft}

% \pagestyle{fancy}

% \rhead{\emph{PROPOSAL}}
% \lhead{Generic persistency of Haskell data types}
% \renewcommand{\headrulewidth}{0.4pt}

% \addtolength{\textheight}{1.2in}
% \addtolength{\voffset}{ -0.6in}
% \addtolength{\textwidth}{1.0in}
% \addtolength{\hoffset}{ -0.5in}
% \setlength{\parindent}{0in}
% \setlength{\parskip}{\baselineskip}
% \setlength{\columnsep}{0.4in}

% \setlength{\columnseprule}{0.001in}
% \addtolength{\evensidemargin}{ -0.2in}
% \addtolength{\oddsidemargin}{ -0.2in}
% \setlength{\itemindent}{0in}
% \setlength{\itemsep}{0in}

\definecolor{stress}{rgb}{0.50,0.50,0.50} 
\newcommand{\stress}[1]{\textcolor{stress}{#1}}

\title{PROPOSAL: Generic persistency of Haskell data types}
\author{Sebastiaan Visser}
\date{\today}

% -----------------------------------------------------------------------------

\begin{document}
\maketitle
\abstract{}
\pagebreak
\tableofcontents 
\pagebreak

\input{intro}        \pagebreak
\input{overview}     \pagebreak
\input{framework}    \pagebreak
\input{motivation}   \pagebreak
\input{goal}         \pagebreak
\input{related-work} \pagebreak

\bibliographystyle{plain}
\bibliography{proposal}
\end{document}

