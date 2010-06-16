%\conferenceinfo{WXYZ '05}{date, City.} 
%\copyrightyear{2005} 
%\copyrightdata{[to be supplied]} 

%\titlebanner{banner above paper title}        % These are ignored unless
%\preprintfooter{short description of paper}   % 'preprint' option specified.

\title{Generic Storage in Haskell}
%\subtitle{.}

\authorinfo{Sebastiaan Visser}
           {Utrecht University}
           {sebastiaan@@fvisser.nl}
\authorinfo{Andres L\"oh}
           {Utrecht University}
           {andres@@cs.uu.nl}

\maketitle

\begin{abstract}
We present a framework to define data structures that can be used on disk.
The structure resides in a heap in a file on the disk. Operations
read and write only the parts of the data structure that are actually
needed. The framework is based on expressing datatypes as fixed points
of functors and then annotating the recursive positions with additional
information. We explain how functions, if expressed in terms of standard
recursion patterns, can be easily lifted from a pure setting
to an effectful, annotated scenario. As a running example, we sketch
how to implement a persistent library of finite maps based on binary
search trees.
\end{abstract}

% \category{CR-number}{subcategory}{third-level}

% \terms
% term1, term2

% \keywords
% keyword1, keyword2

