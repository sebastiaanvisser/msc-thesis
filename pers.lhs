%include polycode.fmt

\section{Persistent containers}

\fancy{The most complex part of the project will be to allow the usage of
purely functional container data types as a transparent projection on the
persistency layer.  Most -- or possibly all -- container data structures are
based on recursive data types; data types that in one or more of their
constructors refer back to themselves.  By making both the recursive points in
the container definitions and the recursion in the container algorithms
explicit using a fix-point combinator, we can freely annotate the container's
behaviour. This implies we are able to generically hook in our own functions
where the original algorithm would go into recursion. By hooking in functions
that read or write parts of the container and the container data from file, we
can transparently persist the data structure.}

\fancy{To make things more clear consider the following example of a binary
tree |FTree|, parametrized with a type variable |a| for the key type, type
variable |b| for the value type and an |f| for the recursive points.}

>data FTree a b f = Leaf | Branch a b f f

\fancy{Now we can tie the knot using a fix-point combinator and get a back a
proper |Tree| data type. This tree is still indexed with the key and value
types, the recursive points are regular sub-trees packed in a |Fix|.}

>newtype Fix f = In {out :: f (Fix f)}
>newtype Tree a b = Tree { ftree :: Fix (FTree a b) }

\fancy{Now we are able to write the common binary tree lookup function, but
with some specialized machinery to deal with recursion. The function takes,
among the key to search for, a function to lift values into an annotated lookup
result and a function that deals with the recursive lookup. We call this
function |lookupA| (the A of Annotated) because it must still be parametrized
with the helper functions before it can be used.}

>lookupA :: (Ord a, Monad m) =>
>      a            -- key to search for
>  ->  (m b -> c)   -- lifter for query result
>  ->  (f -> c)     -- recursive annotated lookup
>  ->  FTree a b f  -- tree to search in
>  ->  c            -- lifted query result
>lookupA _ p _ Leaf = p (fail "not found")
>lookupA a p f (Branch c d l r) =
>  case a `compare` c of
>    EQ  -> p (return d)
>    LT  -> f l
>    GT  -> f r

\fancy{This function is a so called query function because it extracts a value
from a container.  Because we left the choice for the recursive part of the
|lookupA| function open, we can now tie the knot on several ways. The
|monadicQ| function generically lifts a query function to a monadic
computation. This monadic computation can be parametrized with a custom
recursive processor function that describes how we can get to the
sub-containers.}

>monadicQ :: Monad m  -- the monad we live in
>  => (t -> m a)      -- lifted unwrap function
>  ->   ((c -> m c)   -- lift function
>     -> (t -> m b)   -- post processor
>     -> a            -- recursive container to query in
>     -> m b)         -- recursive lifted query result
>  -> t               -- container to query in
>  -> m b             -- lifted query result
>monadicQ p q c = p c >>= q return (monadicQ p q)

\fancy{Although the type signature can be a bit scary at first sight, the usage
is rather straightforward.  To make clear what we can do with a combination of
such a specific, annotation aware, query function like |lookupA| and a generic
monadic lifter function we introduce the |lookupTrace| function. This function
annotates the |lookupA| with a monadic computation that prints out the piece of
the container (sub-tree) we are currently dealing with. The result is a lookup
function that prints out a trace of the spine of the container it traverses.}

>lookupTrace :: (Ord a, Show a, Show b) =>
>  a -> Tree a b -> IO (Maybe b)
>lookupTrace a = monadicQ proc (lookupA a) . ftree
>  where proc c = print c >> return (out c)

\fancy{In the previous example we plugged in a |Fix FTree| at the recursive
points of |FTree| to get back a real binary tree. Instead of immediately
plugging in sub-trees we can also plug in a |Pointer| to a location of the
sub-trees on the persistent Heap.}

>type PTree a b = FTree a b Pointer

\fancy{Now we should be able to use the same annotation technique as before,
but this time we do not unpack the fix-point and print a trace, but read the
next (partial) sub-tree from disk and continue the lookup.}

>lookupP :: Ord a => a -> PTree a b -> IO (Maybe b)
>lookupP = monadicQ (deserialize `liftM` read) . lookupA

\fancy{This trick will be the basis of a more or less transparent system for
data type persistency. Lots of research can be invested into finding ways to
make the usage as clean as possible without losing to much expressiveness.}

\fancy{In the example above we have only sketched how the technique can work
for query functions, the same ideas can be used for producer and modifier
functions.}

