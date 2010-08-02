%\conferenceinfo{WXYZ '05}{date, City.} 
%\copyrightyear{2005} 
%\copyrightdata{[to be supplied]} 

%\titlebanner{banner above paper title}        % These are ignored unless
%\preprintfooter{short description of paper}   % 'preprint' option specified.

\title{Generic Storage in Haskell}
%\subtitle{.}

\authorinfo{Sebastiaan Visser \and Andres L\"oh}
           {Department of Information and Computing Sciences\\ Utrecht University}
           {sebastiaan@@fvisser.nl \and andres@@cs.uu.nl}

\maketitle

\begin{abstract}
We present a framework for constructing functional data structures that can be stored on disk.
The data structures reside in a heap saved in a binary file. Operations
read and write only the parts of the data structure that are actually
needed. The framework is based on expressing datatypes as fixed points
of functors and then annotating the recursive positions with additional
information. We explain how functions, if expressed in terms of standard
recursion patterns, can be easily lifted from a pure setting
to an effectful, annotated scenario. As a running example, we sketch
how to implement a persistent library of finite maps based on binary
search trees.
\end{abstract}

\category{D.2.13}{Reusable Software}{Reusable libraries}

\terms
Languages

\keywords
datatype-generic programming, fixed points, annotations
