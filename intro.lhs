%include polycode.fmt

\section{Introduction}

\draft{

Session types have been described for concurrent systems like the pi-calculus
and are explained as primitive additions to regular type system.  Session types
are implemented in Haskell twice by embedding them into the language make using
of the expressiveness of the type system, not by extending it. The first
implementation is from 2004 and described in [todo]. The second implementation
is from 2008 and described in [todo].

}

\begin{itemize}

  \item \BETA{

  In section 2 we describe two well known approaches to channel based
  communication that are commonly used in concurrent Haskell programs. Both
  ways have their own strengths and weaknesses, but both are essentially not
  type safe. We point out where the problems are as a bridge to our explanation
  of a type safe approach. We give a two example implementations of a simple
  e-mail client for both approaches.

  }

  \item \BETA{

  In section 3 we introduce the concept of session types and give a common
  notation to write these down. We give a sample session type for the example
  session introduced in section 2. We show how to compute the dual of a session
  type and illustrate this for our example session.

  }

  \item \draft{

  In section [todo] we will describe and compare two implementations of session
  types in Haskell.  The first implementation is from 2004 and developed by
  Neubauer and Thiemann and this is a rather complicated library that makes
  extensively use of type classes. In this paper we refer to this library as
  the \emph{original implementation}. The second implementation is from 2008
  developed by Pucella and Tov, their implementation is significantly simpler.
  In this paper we will refer to this library as the \emph{simple
  implementation}.

  Both paper present a calculus for asynchronous communication using session
  types and describe how to embed this calculus into Haskell. While both papers
  also show correctness for their embedding, in this paper will only explain
  their correctness informally.

  }

\end{itemize}

