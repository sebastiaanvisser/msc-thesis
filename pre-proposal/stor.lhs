%include polycode.fmt

\section{Storage}

Data in a program in not rigid, during the lifetime of the program data might
get extended modified or deleted. To reflect these possibilities to a
persistent storage, we need something flexible like the heap in regular program
memory.  Storage heaps allow us to freely \emph{allocate} of \emph{free} blocks
of contiguous memory and \emph{read} from- and \emph{write} data to these
blocks.  The following interface for an abstract storage heap should be
sufficiently flexible for the high level persistency layer.

>allocate  :: Heap -> Size -> IO Pointer
>free      :: Heap -> Pointer -> IO ()
>read      :: Heap -> Pointer -> IO ByteString
>write     :: Heap -> Pointer -> ByteString -> IO ()

To make data persistent in a regular file on the filesystem we need a
constructor function like this:

>newFileHeap :: FilePath -> IO Heap

This interface should allow us to freely navigate, grow and shrink the data
wihtout touching the entire file. This is essential when dealing with large
amounts of data. The exact details of the implementation remain unknown to
users of the heap.

