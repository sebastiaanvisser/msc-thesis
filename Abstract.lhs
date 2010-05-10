%include polycode.fmt
%include thesis.fmt

\chapter*{Abstract}

Algebraic datatypes in Haskell are a powerful tool to structure application data.
Unfortunately, Haskell datatypes can only be used in-memory.
When information is too large to fit in application memory or has to survive the running time of a single process, the information has to be marshalled to and from a persistent storage.
The current solutions for persistent storage in Haskell have some drawbacks.
Either the structure of these storage systems does not fit the algebraic datatypes of Haskell well or the systems only allow to access the persistent storage as a whole.
In this document we describe a new storage framework for Haskell that uses generic programming to allow the persistent storage of arbitrary functional data structures.
The system allows incremental access to individual non-recursive nodes in a recursive data structure.
Incremental access allows the operations on the persistent data structures to run with the same asymptotic running time as their in-memory counterparts.
The framework uses a generic annotation system to automatically lift operations on recursive data structure to work on a block based storage heap on disk.
The annotation system helps to keep a strict separation between I/O code and the pure operations on functional data structures.
The system we introduce enables the use of both general purpose and domain specific functional data structures as a persistent storage for application data.
A clean distinction between the low-level physical storage and the high-level functional data structures makes sure the interface to the user remains purely functional.  
