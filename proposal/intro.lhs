%include polycode.fmt

\section{Introduction}

This document describes the background, motivation, goals and planning of my
MSc thesis project.  The next month I will be working on this project with the
help of my supervisor Andres L\"oh.

This document contains a proposal for a framework to allow persistence of
Haskell data in a way not currently available to Haskell.  The second section
section will give a brief technical introduction on the possibilities of data
type generic programming with a fixed point view on data types.  This
demonstrates how to create data structures that are open to external
annotation, which will be the basis of this framework.  The third section will
give a motivation for this project by pointing out the currently available
options for data persistence in Haskell and show why this options are not
sufficient.  The fourth section will try to explicitly specify what primary
goals should be reached for this to succeed.  The fifth section gives a
planning for the upcoming months.  The sixth section summarizes the additional
work that can be done to extend this framework.  The last section on related
work gives a broad overview of things already achieved related to this topic
and explicitly states what we can learn from it.

