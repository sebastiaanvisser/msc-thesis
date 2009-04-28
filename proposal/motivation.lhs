%include polycode.fmt

\section{Motivation}

  Currently there are several ways of making Haskell data structure persistent
  on external storage disks. Long-lived information storage is an essential
  ingredient in a large amount of modern applications. Although there is a lot
  to learn from existing tools, there are currently no work available that
  satisfies the goals specified earlier.

  \subsection{Relational Database Management Systems}

    Connection to existing relational database management system do not suffice
    because it is hard to project the structure of arbitrary functional data
    structures to the table based layout of such a system.

    It is not hard to imagine the possibility of a automated conversion from a
    generic data representation to database rows. The problem with such a
    conversion is the quite heavy dependency on external database tools and the
    rather inefficient mapping from algebraic data types to tables flat tables.

