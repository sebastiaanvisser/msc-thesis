%include polycode.fmt

\section*{Introduction}

This document describes the background, motivation, goals, and planning of my
MSc thesis project.  The next months I will be working on this project with the
help of my supervisor Andres L\"oh.

This document contains a proposal for a framework for transparent persistence
of arbitrary data structures in the pure and functional programming language
Haskell.  The proposed framework stresses the reuse of existing Haskell data
types and algorithms for long term storage on external devices.

The first section will give a motivation for this project by pointing out the
currently available options for data persistence in Haskell.  Additionally this
section will point out why these options might not be sufficient in all cases.
The second section will explicitly state what goals should be reached for the
MSc project to succeed and what alternative options there are in the case of
getting stuck somewhere in between.  The third section will give a brief
technical introduction to the possibilities of data type generic programming
with a fixed point view on data types.  This section demonstrates how to create
data structures that are open to external annotation, which will be the basis
of the framework.  The fourth section gives a global planning for the upcoming
months.  The fifth section summarizes the additional work that can be done to
extend the framework.  The last section on related work gives a broad overview
of things already achieved in the field related to this project.  The influence
of related work on this project will be explained topic by topic.

