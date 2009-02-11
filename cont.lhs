%include polycode.fmt

\section{Containers}

\draft{In haskell you can have data types of different structure;
\emph{primitive values} like characters and integers, \emph{composed values}
like a user containing a name, address and country field, and \emph{container
data t} like lists, maps, trees, etc.  }

\draft{When dealing with larger collection of data it is convenient to wrap
your primitive or composed values in a container, or a composition of multiple
containers.}

\draft{For example, a list of mappings from strings a binary tree of integers.  }

\draft{
>type MyCollection = [Map String (Tree Int)
}

\draft{Persisting all data in that collection, including the collection
adminstration itself, on the an external storage device and still want to apply
regular functions like lookup, update and delete on it transparently is a bit
involved.}

\draft{Inspired by how data is stored in-memory and using some clever generic
programming techniques it should be possible to hide most details of
persistency from the user.}

\begin{itemize}

  \item
  \draft{When need some sort of heap mechanism on the storage backend with
  which we can allocate new blocks of data and free unused block of data. We
  need to be able to serialize pointer values to these data blocks to allow
  references between different data values.}

  \item
  \draft{We need to be able to serialize and deserialize values of every data
  type generically to streams of bits generically.}

  \item
  \draft{We need to be able to track `read' and `write' actions of function
  working on collection algorithms to be able to lift these actions to the
  world of persistency. This lifting process should be invisible to the users
  of the system and at least be easy to use for writers of container data
  types.}

\end{itemize}


