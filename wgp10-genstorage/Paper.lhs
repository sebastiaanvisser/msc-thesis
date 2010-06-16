\documentclass[preprint]{sigplanconf}

\usepackage{todonotes}
\newcommand\andres[2][]{\todo[color=blue!40,size=\footnotesize,#1]{#2}}
\setlength\marginparwidth{1.7cm}

\usepackage{amsmath}
\usepackage{mathptmx}
\usepackage{natbib}

%include polycode.fmt
%include forall.fmt
%include paper.fmt

% For indentation in verbatim blocks:
\newcommand\gobble[1]{\hspace*{\mathindent}}

\begin{document}

%include Titlepage.lhs
%include Introduction.lhs
%include Fixpoints.lhs
%include Morphisms.lhs
%include Heap.lhs
%include Storage.lhs
%include Extras.lhs
%include RelatedWork.lhs
%include Appendix.lhs

\bibliographystyle{abbrvnat}
\bibliography{paper}

\end{document}

