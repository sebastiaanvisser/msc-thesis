%include polycode.fmt

\section{Storage}

\draft{When data needs to be stored on the external device and must be queried,
modified and extended on the fly without touching the entire data store at once
we needs something analoge to the heap used in-memory.}

\draft{
>allocate  :: Heap -> Size -> IO Pointer
>free      :: Heap -> Pointer -> IO ()
>read      :: Pointer -> IO ByteString
>write     :: Pointer -> ByteString -> IO ()
}

\draft{This interface should allow us to freely navigate, grow and shrink the
data wihtout touching the entire file. This is essential when dealing with
large amounts of data.}


