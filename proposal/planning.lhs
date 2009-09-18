%include polycode.fmt

\section{Planning}

\begin{itemize}

\item[done]

Implement an initial prototype that shows the main concept without going to
much into detail. This includes an storage heap on disk and a one simple
persistent container.

\item[done]

Work on this proposal in the mean time put some effort in research on the topic
of abstracting away recursion.

\item[in progress]

Start playing around with generic programming libraries and try to find a clean
way to write data structures that are open for annotation. Figure out how this
can be done without bothering container developers with any boilerplate. Write
thesis chapter on this topic.

This part might be the most interesting part of this project because it will
constitute the bridge between the framework and the user code. There might not
be `a best solution' for this part and not too much time should be spent on
this because work on the succeeding topics may gain valuable insides.

\item[in progress]

Use the knowledge gathered in the previous month to write one general purpose
data structure that gets lifted to the persistent storage. In the mean time
work on the structure and usage of this framework from the user's perspective.
Write thesis chapter on this topic.

\item[...]

Work on garbage collection and sharing. Start out with implementing a
conservative reference counting garbage collection. Try to find simple example
of data structures that share values between multiple recursive sub structures
to play with this. Write thesis chapter on this topic.

When this subject is less easy to implement than expected try to find a way to
overall prevent sharing. This is undesirable but a safe alternative.

When implementing a conservative garbage collection is more easy than expected
and can easily be shown effective for non-cyclic data structures, try to
implement a non-conservative garbage collector. This might show some
interesting implications on the framework.

\item[...]

Work on implementing one special purpose data structure. Write thesis chapter
on this topic. When this takes less time than expected, because knowledge of
the implementation of the general purpose data structure can be used, try to
get type hashes to work.

\item[...]

Try to implement or reuse an existing transactional cache both to try to
improve performance and to allow concurrent access. Write thesis chapter about
this.

\item[\emph{2009}]

Finish up thesis.

\end{itemize}


