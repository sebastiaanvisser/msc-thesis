\section{Conclusion}\label{sec:conclusion}

The power of programming languages is often correlated with the exposed
standard libraries. Mainstream languages all provide their own set of data
structures that the programmer can be used to manage information in application
memory. Using the same structures to manage information on external storage
devices is generally not possible.

With this work we provide the possibility to use functional data structures
implemented in Haskell to manage information outside application memory.  As a
low-level storage we use a heap structure that stores blocks of binary data on
a file on disk. The heap can grow and shrink on demand.  Operations traversing
persistent recursive data structures read or write non-recursive nodes from and
to the heap level by level. The incremental behaviour keeps the algorithms
efficient, the asymptotic running time on disk is equal to that in-memory.

Writing persistent data structures requires to abstract away from recursion in
both the definitions of the datatypes and the definitions of the operations.
The operations abstract away from recursion using recursion patterns like the
catamorphism and the anamorphism. The algebras used as a description for the
recursions operations remain pure and annotation agnostic.

Working with persistent data structures is not very different from working with
normal data structures, although all operations need to be lifted to the
monadic |Heap| context.

We encourage writing recursive datatype definitions as pattern functors and
like to see operations that abstract away from recursion. Datatypes written
this way are open to annotation which can be exploited in a variety of ways.

% \appendix
%
% \acks
% 
% Acknowledgments, if needed.
%
% We recommend abbrvnat bibliography style.
% 
% \bibliographystyle{abbrvnat}
% 
% The bibliography should be embedded for final submission.
% 
% \begin{thebibliography}{}
% \softraggedright
% 
% \bibitem[Smith et~al.(2009)Smith, Jones]{smith02}
% P. Q. Smith, and X. Y. Jones. ...reference text...
% 
% \end{thebibliography}

