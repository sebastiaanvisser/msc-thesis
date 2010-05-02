%\documentclass{article}
%\documentclass[12pt]{scrartcl}
%\documentclass[a4wide,10pt]{article}
\documentclass[11pt,twoside,a4paper,openright]{report}


%\usepackage[disable]{todonotes}
\usepackage[pdftex]{graphicx}
\usepackage{xcolor}
\usepackage{amsmath}
\usepackage{url}
\usepackage{draft}
\usepackage{fancyhdr}
\usepackage{pxfonts}
\usepackage[authoryear,sort,square]{natbib}
\definecolor{linkblue}{RGB}{6,55,206}
\usepackage[colorlinks=true,linkcolor=linkblue]{hyperref} %m must be last \usepackage

\setlength{\parindent}{0pt}
\setlength{\parskip}{0.1in} % \baselineskip}
% \addtolength{\voffset}{-0.4in}
% \addtolength{\textheight}{0.6in}
% \addtolength{\hoffset}{-0.5in}
% \addtolength{\textwidth}{1.0in}

\pagestyle{fancy}
\renewcommand{\sectionmark}[1]{\markright{\thesection.\ #1}}
\renewcommand{\chaptermark}[1]{\markboth{\ #1}{}}   % \thechapter

\bibliographystyle{alpha}

%include polycode.fmt
%include thesis.fmt
%include haskell.fmt
%include forall.fmt

%if False

> module Thesis where
> import Fixpoints ()
> import Morphisms ()
> import Example ()
> import Heap ()
> import Storage ()
> import HigherOrder ()

%endif

\begin{document}

  %include Titlepage.lhs
  %include Abstract.lhs
  \tableofcontents
  %include Introduction.lhs
  %include Fixpoints.lhs
  %include Morphisms.lhs
  %include Heap.lhs
  %include Storage.lhs
  %include HigherOrder.lhs
  %include RelatedWork.lhs
  %include FutureWork.lhs
  %include Conclusion.lhs
  \bibliographystyle{plain}
  \bibliography{thesis}

\end{document}
