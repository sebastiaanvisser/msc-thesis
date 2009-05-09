%include polycode.fmt

\abstract{
  \fancy{
  Algebraic data types (ADTs) are a powerful way to structure data in Haskell,
  functions operating on this data can be used to process values of these ADTs.
  When dealing with large data sets that do not fit into computer memory at
  once, tricks have to be used to persist data on external storage devices.
  There are several techniques for data persistence available for Haskell,
  unfortunately none of them truly transparent to the user or not allowing
  incremental access.  This document proposes a new persistence framework for
  Haskell that uses purely functional data structures to manage long lived data
  on disk.  No external database tools are used so the system remains
  lightweight and does not compromise the functional paradigm.
  }
}
 
