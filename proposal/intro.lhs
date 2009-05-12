%include polycode.fmt

\section{Introduction}

This document describes the background, motivation, goals and planning of my
MSc thesis project.  The next months I will be working on this project with the
help of my supervisor Andres L\"oh.

This document contains a proposal for a framework for transparent persistence
of arbitrary data structures in the pure and functional programming language
Haskell. The proposed framework stresses the reuse of existing Haskell data
types and algorithms for long term storage on external devices.

The second section will give a brief technical introduction on the
possibilities of data type generic programming with a fixed point view on data
types. This section demonstrates how to create data structures that are open to
external annotation, which will be the basis of this framework.  The third
section will give a motivation for this project by pointing out the currently
available options for data persistence in Haskell. Additionally this section
will point out why these options might not be sufficient in all cases.  The
fourth section will try to explicitly specify what goals should be reached for
this MSc project to succeed and what alternative options there are in the case
of getting stuck somewhere in between.  The fifth section gives a global
planning for the upcoming months.  The sixth section summarizes the additional
work that can be done to extend this framework.  The last section on related
work gives a broad overview of things already achieved in the field related to
this topic and explicitly states what we can learn from it.

