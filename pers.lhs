%include polycode.fmt

\section{Persistent containers}

\draft{The hardest part of the project will be the to allow the usage of pure
functional container data types as a projection on the persistency layer. Most
-- or possibly all -- container data structures are based on recursive data
types; data types that in one or more of their constructors refer back to
themeselve. By making both the recursive points in the container definitions
and the recursion in the container algorithms explicit using a fix-point
combinator, we can freely annotate the container's behaviour.  This means that
we are able to generically hook in our own functions where the original
algorithm would go into recursion. By hooking in functions that read or write
parts of the container and container data from file we can transparently
persist the data structure.}

\draft{To make things more clear, consider the following example of a binary
tree |FTree| parametrized with type variabel |f| for the recursive points. The
tree is parametrized with a key type |a| and a value type |b|}

\draft{
>data FTree a b f = Leaf | Branch a b f f
}

\draft{Now we can, among other things, tie the knot using a fix-point
combinator and get a back a proper |Tree| data type indexed with the key and
value types.}

\draft{
>newtype Fix f = In {out :: f (Fix f)}
>newtype Tree a b = Tree { ftree :: Fix (FTree a b) }
}

\draft{Now we are able to write the common binary tree lookup function with
some specialized machinery to deal with recursion. The function takes, among
the key to search for, a function to lift values into an annotated query result
and a function that deals with the recursive lookup.}

\draft{
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
}

\draft{Because we left the choice for the recursive part of the |lookupA|
function open, we can tie the knot on several ways. The following function
generically lifts a query function -- from a container type to a query results
-- to a monadic computation. This monadic computation can be parametrized with
custom processor functions that describes how we can get to the sub-containers.
}

\draft{
>monadicQ :: Monad m
>  => (t -> m a)      -- Lifted unwrap function.
>  ->   ((c -> m c)   -- Lift function.
>     -> (t -> m b)   -- Post processor.
>     -> a     -- Recursive container to query in.
>     -> m b)  -- Recursive lifted query result.
>  -> t        -- Container to query in.
>  -> m b      -- Lifted query result.
>monadicQ p q c = p c >>= q return (monadicQ p q)
}

\draft{To make clear what can do with the combination of such a specific,
annotation aware, query function like |lookupA| and a generic monadic lifter
function we introduce the |lookupInIO| function. This function annotates the
|lookupA| with a monadic computation that prints out the piece of the container
(sub-tree) we are currently dealing with. The result is a lookup function that
prints out a trace of which part of the container it traverses.}

\draft{
>lookupInIO :: (Ord a, Show a, Show b) =>
>  a -> Tree a b -> IO (Maybe b)
>lookupInIO a = monadicQ proc (lookupA a) . out
>  where proc = (\c -> print c >> return c) . out
}

\draft{In the previous example we plugged in the |Fix FTree| into the recursive
points of |FTree| to get back a real binary tree. We can now plug in not a real
tree but a |Pointer| to a location of the sub-trees on the persistent Heap.}

\draft{

>type PTree a b = FTree a b Pointer

}

\draft{Now we should be able to use the same annotation technique as before,
but this time we do not unpack the fix-point and print a trace, but read the
next (partial) sub-tree from disk and continue the lookup.}

\draft{
>lookupP :: Ord a => a -> PTree a b -> IO (Maybe b)
>lookupP = monadicQ (deserialize `liftM` read) . lookupA
}

\draft{This trick will be the basis of a more or less transparent system for
data type persistency. Lots of research can be invested into finding ways to
make the usage as clean as possible without losing to much performance.}

\draft{In the example above we have only sketched how the systems work for
query functions, the same ideas can be used for producer and modifier
functions.}


