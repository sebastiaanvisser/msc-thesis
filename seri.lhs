%include polycode.fmt

\section{Generic serialization}

\draft{In order to be able to save more than just binary data we need to
perform binary serialization. This function should work for all data that we
can view generically.}

\draft{
>serialize    :: View a => a -> BitStream
>deserialize  :: View a => BitStream -> Maybe a
}

\draft{There are several libaries available for generic serialization. It might
be interesting to investigate which of them is most suitable to our needs.}

