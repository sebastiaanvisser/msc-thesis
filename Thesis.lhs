%\documentclass{article}
%\documentclass[12pt]{scrartcl}
%\documentclass[a4wide,10pt]{article}
\documentclass[12pt, a4paper]{report}

%include polycode.fmt

\usepackage[pdftex]{graphicx}
\usepackage{xcolor}
\usepackage{amsmath}
%\usepackage[disable]{todonotes}
\usepackage{url}
\usepackage{draft}
\usepackage{fancyhdr}
\usepackage{pxfonts}
\usepackage[authoryear,sort,square]{natbib}
% \usepackage{mathpazo} % For the fancy font, idea from Martijn/Wouter
\definecolor{linkblue}{RGB}{6,55,206}
\usepackage[colorlinks=true,linkcolor=linkblue]{hyperref} %m must be last \usepackage

\setlength{\parindent}{0pt}
\setlength{\parskip}{0.1in} % \baselineskip}


% \pagestyle{fancy}
\renewcommand{\sectionmark}[1]{\markright{\thesection.\ #1}}
\renewcommand{\chaptermark}[1]{\markboth{\ #1}{}}   % \thechapter
%\lhead{Generic persistence of Haskell data types}

\bibliographystyle{alpha}

% \addtolength{\voffset}{-0.4in}
% \addtolength{\textheight}{0.6in}
% \addtolength{\hoffset}{-0.5in}
% \addtolength{\textwidth}{1.0in}


%if False

> module Thesis where
> import Fixpoints ()
> import Morphisms ()
> import Example ()
> import Heap ()
> import Storage ()
> import HigherOrder ()

%endif

\title{A GENERIC APPROACH TO DATATYPE PERSISTENCY IN HASKELL}
\author{Sebastiaan Visser\\
\small{Utrecht University}\\
\small{\texttt{s@@fvisser.nl}}
}
\date{\today}

\begin{document}

%\maketitle
%\input{Abstract}
%
%\tableofcontents
%
%\input{Introduction} 
%\input{Motivation} 
%\input{Overview} 
\input{Fixpoints} 
% \input{Morphisms} 
% \input{Heap} 
% \input{Storage} 
% \input{Example} 
% \input{HigherOrder} 
% \input{RelatedWork} 
% \input{Conclusion}
% \input{FutureWork} 

\newpage
\bibliographystyle{plain}
\bibliography{thesis}

\end{document}

