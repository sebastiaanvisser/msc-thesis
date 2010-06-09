%-----------------------------------------------------------------------------
%
%               Template for sigplanconf LaTeX Class
%
% Name:         sigplanconf-template.tex
%
% Purpose:      A template for sigplanconf.cls, which is a LaTeX 2e class
%               file for SIGPLAN conference proceedings.
%
% Author:       Paul C. Anagnostopoulos
%               Windfall Software
%               978 371-2316
%               paul@windfall.com
%
% Created:      15 February 2005
%
%-----------------------------------------------------------------------------

\documentclass[preprint]{sigplanconf}

% The following \documentclass options may be useful:
%
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

\usepackage{todonotes}
\usepackage{amsmath}
\usepackage{mathptmx}
\usepackage{natbib}

%include polycode.fmt
%include forall.fmt
%include paper.fmt

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

