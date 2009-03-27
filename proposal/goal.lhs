%include polycode.fmt

\section{Goals}

  The main goal of this project is to create a framework for the programming
  language Haskell which enables transparent persistence of values of arbitrary
  data types to an external storage device.

  The key aspects of this framework will be:

  \begin{itemize}
    \item The mapping of arbitrary data structures to disk.
    \item Use lifted version of pure data structures. 
    \item High performance by incremental reads/writes.
    \item Generic marshalling of values to binary data.
    \item No dependency on third-party database tools.
  \end{itemize}

  \subsection{Domain specific containers}

  The framework should not make any assumptions on the structure of the
  information that is made persistent. This allows the usage of arbitrary
  domain specific data structures which their own time and space complexity
  optimized for a specific application.

  \subsection{Pure algorithms}

  Developers writing container data types to be used in the persistence
  framework should not be bothered with the inner workings of the system. It is
  the responsibility of th framework to lift pure data structures into the IO
  monad with the correct read and write operations, not the responsibility of
  the programmer.

  \subsection{Incremental IO access}

  \subsection{Generic binary serialization}

  \subsection{Domain specific containers}

