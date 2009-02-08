{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Generic.Annotate where

newtype Fix f = In {out :: f (Fix f)}

instance Show (f (Fix f)) => Show (Fix f) where
  show = const "..." -- ("[| " ++) . (++ " |]") . show . out

-- Tying the knots for recursive computations.

-- Modifications.
fixM :: ((Fix f -> Fix f) -> f (Fix f) -> f (Fix f)) -> Fix f -> Fix f
fixM m = In . m (fixM m) . out

-- Queries.
fixQ :: ((Fix f -> a) -> f (Fix f) -> a) -> Fix f -> a
fixQ q = q (fixQ q) . out

-- Annotated queries.
annQ
  :: (Fix f -> t)                          -- Tree indexed lift function.
  -> (Fix f -> b -> c)                     -- Tree indexed post processor.
  -> (t -> (Fix f -> c) -> f (Fix f) -> b) -- Real query function.
  -> Fix f                                 -- Container to query in.
  -> b                                     -- Annotate query result.
annQ l p q c = q (l c) (p c . annQ l p q) (out c)

traceQ
  :: ((t -> (t, [a])) -> (Fix f -> (b, [a])) -> f (Fix f) -> (b, [a]))
  -> (f (Fix f) -> a)
  -> Fix f
  -> (b, [a])
traceQ q s = annQ lift post q 
  where lift c a      = (a, [s (out c)])
        post c (a, b) = (a,  s (out c) : b)

ioQ
  ::   ((a -> IO a)    -- lift function
    -> (Fix f -> IO b) -- post processor
    -> f (Fix f)       -- recursive container
    -> IO b)           -- recursive query result
  -> Fix f             -- container
  -> IO b              -- query result in IO
ioQ q c = do
  print "trace"
  let c'   = out c
      lift = return
      post = ioQ q
  q lift post c'

