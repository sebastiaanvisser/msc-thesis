%include polycode.fmt

\section{Collections}

\review{In haskell you can have many data types of different structure;
\emph{primitive values} like characters and integers, \emph{composed values}
like a user containing a name, address and country field, and \emph{container
data types} like lists, maps, trees, etc. When dealing with larger collection
of data it is convenient to wrap your primitive or composed values in a
container, or a composition of multiple containers.}

\review{For example, a list of mappings from strings a binary tree of |User|s:}

>type MyCollection = [Map String (Tree User)]

\review{Persisting all data in that collection, including the collection
administration itself, to an external storage device and still having the
ability to perform regular actions on it (like |lookup|, |update|, |delete|,
etc.) is a bit involved. Inspired by how normal Haskell data is represented in
memory and using some clever generic programming techniques we should be
possible to hide most details of persistency from the user.}

\begin{itemize}

  \item
  \review{We need some sort of heap mechanism on the storage back-end on
  which we can allocate new blocks of data and free unused block of data. We
  need to be able to serialize pointer values to these data blocks to allow
  references between different data values.}

  \item
  \review{We need to be able to serialize and deserialize values of every data
  type to streams of bits generically. }

  \item
  \review{We need to be able to track `read' and `write' actions of algorithms
  working on collections to be able to lift these actions to the world of
  persistency.}

\end{itemize}


