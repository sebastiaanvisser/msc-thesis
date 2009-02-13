%include polycode.fmt

\section{Generic serialization}


Using the generic storage heap for which we defined the interface in the
previous section allows us to read and write arbitrary blocks of binary data.
This binary data is stored in the efficient |ByteString| type. When we want to
be able to store values of any type, not only bit-sequences, there is a need
for a generic serialization and deserialization algorithm. Two simple generic
functions are all that we need to convert data from type Haskell values to
streams of bits and vice versa:


>serialize    :: a -> ByteString
>deserialize  :: ByteString -> Maybe a

Or, when we are sure the data must be of the type we expect so the bit patterns
match:

>unsafeDeserialize  :: ByteString -> a

There are several libraries available that can be used for generic
serialization, all of them having there own advantages and disadvantages. It is
interesting to investigate which of generic programming libraries
available\cite{compgen} fits our needs best. Writing a generic reader and
writer function should not be very hard and has been described in detail in
existing literature\cite{genhaskellpt, emgm, printparse}.

