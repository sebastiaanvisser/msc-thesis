\documentclass{sigplanconf}

\usepackage{todonotes}
\newcommand\andres[2][]{\todo[color=blue!40,size=\footnotesize,#1]{#2}}
\setlength\marginparwidth{1.7cm}

\usepackage{ae}
\usepackage{amsmath}
\usepackage[scaled=0.9]{helvet}
\usepackage{mathptmx}
\usepackage{natbib}
\usepackage[T1]{fontenc}

%include polycode.fmt
%include forall.fmt
%include paper.fmt

% For indentation in verbatim blocks:
\newcommand\gobble[1]{\hspace*{\mathindent}}

% Smaller code blocks:
\renewcommand\hscodestyle\small

% Font selection:
\let\Varid\mathsf
\let\Conid\mathsf
\newcommand\Keyword[1]{\textbf{\textsf{#1}}}
\let\Numeral\mathsf
\DeclareMathAlphabet{\mathsf}{OT1}{phv}{m}{sf}


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

