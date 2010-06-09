\section{Introduction}

Describe the problem:

\begin{itemize}

\item - Algebraic datatypes in Haskell provide a powerful way to structure data. \\
      - Recursive datatypes can be used to create functional data structures.
\item - Unfortunately there are no good way to store functional data structures on disk. \\
      - We identify three important properties of a functional persistence framework:
      \begin{enumerate}
      \item Efficiency:   Incremental access to parts of the data.
      \item Flexibility:  The system should provide ways to store domain specific data structures.
      \item Transparency: Users should not be bothered with the details and limitations of the underlying system.
      \end{enumerate}

\item Current persistence solutions in Haskell do not provide these three properties:
      \begin{enumerate}
      \item Connections to existing relational database management systems: \\
            - These system allows incremental access, but restrict the users to a table based layout. \\
            - The relational data model is forced upon the user.
      \item Packages for binary serialization like Data.Binary
      \end{enumerate}

\item State your contributions:
      \begin{enumerate}
      \item - We implement a generic framework for working with annotated recursive datatypes. \\
            - The system allows to annotate both data and functionality for all recursive datatype in Haskell.
      \item - We implement a on-disk heap data structure for block based binary storage. \\
            - This heap allows for generic on-demand growing and shrinking binary data storage on disk.
      \item - We use both the generic annotation framework and the heap to derive a generic storage framework. \\
            - The system allow to store arbitrary Haskell data types on disk. \\
            - Recursive datatypes can be accessed incrementally.
      \item - As an example we show how to apply the framework to create a persistent binary tree. \\
            - We indicate the system can even be used for indexed datatypes.
      \end{enumerate}

\end{itemize}

\newpage
