%include polycode.fmt
%include thesis.fmt

%if False

\begin{code}
{-# OPTIONS_GHC -F -pgmF she #-}
{-# LANGUAGE KindSignatures, UndecidableInstances #-}
module Fixpoints where

import Control.Applicative
import Control.Category
import Control.Monad.Reader hiding (mapM)
import Data.Foldable
import Data.Monoid hiding (Endo)
import Data.Traversable
import Prelude hiding ((.), id, mapM)
import Data.Time.LocalTime
\end{code}

%endif

%if False

\begin{code}
fmap' :: Functor f => (a -> b) -> f a -> f b
fmap' = fmap

infixl 6 :+:
infixl 7 :*:

type a :+: b = Either a b
type a :*: b = (a, b)

class (Applicative m, Monad m) => AM m
instance (Applicative m, Monad m) => AM m

fixp :: (t -> t) -> t
fixp a = a (fixp a)
\end{code}

%endif

\begin{section}{Annotated fixed points}

\begin{subsection}{Fixed points}

In Haskell, most container datatypes are written down with explicit recursion.
An example of explicit recursion is the following binary tree datatype. This
binary tree stores both a value and two explicit sub-trees in the branch
constructor, empty trees are indicated by a leaf.

\begin{code}
data Tree_1 v = Leaf_1 | Branch_1 v (Tree_1 v) (Tree_1 v)
\end{code}

\noindent
To gain more control over the recursive positions of the datatype we can
parametrize the binary tree with an additional type parameter used at the
recursive positions. Not the tree datatype itself, but the users of the
datatype mow decide what values to store as sub-trees.

\begin{code}
data Tree_f v f = Leaf | Branch v f f
  deriving Show
\end{code}

\noindent
To get back a binary tree that is isomorphic to our original binary tree that
stored actual sub-trees at the recursive points we can use an explicit fixed
point combinator at the type level. This combinator, conveniently called
|Fix_1|, takes a type constructor of kind |* -> *| and parametrizes this type
with its own fixed point.

\begin{code}
newtype Fix_1 (f :: * -> *) = In_1 { out_1 :: f (Fix_1 f) }
\end{code}

\noindent
By applying the fixed point combinator to the |Tree_f| datatype we get a back a
real binary tree again with real sub-trees at the recursive positions.

\begin{code}
type Tree_2 v = Fix_1 (Tree_f v)
\end{code}

\noindent
To make it easier to deal with the recursive structure of the binary tree we
can make the |Tree_f| an instance of Haskell's |Functor| type class. The
functorial |fmap| lifts the function to be applied against the sub-trees of the
binary tree.

\begin{code}
instance Functor (Tree_f v) where
  fmap _  Leaf            = Leaf
  fmap f  (Branch v l r)  = Branch v (f l) (f r)
\end{code}

\noindent
Besides |Functor| Haskell has two additional type classes that help with
generic traversals over container data types. These are the |Foldable| and
|Traversable| type classes\footnote{Note that these type class instances are
very simple and mechanically derivable. The GHC Haskell compiler version 6.12.1
and above is able to derive the instances for |Functor|, |Foldable| and
|Traversable| for you automatically.}. The foldable type class allows us to
reduce an entire structure into a single value using some |Monoid| operation. 

\begin{code}
instance Foldable (Tree_f v) where
  foldMap _  Leaf            = mempty
  foldMap f  (Branch _ l r)  = f l `mappend` f r
\end{code}

\noindent
The |Traversable| class allows us to traverse the structure from left to right
and perform an actions for each element.

\begin{code}
instance Traversable (Tree_f v) where
  traverse _  Leaf            = (| Leaf |)
  traverse f  (Branch v l r)  = (| (Branch v) (f l) (f r) |)
\end{code}

\noindent
The |Traversable| is very useful because it allows us to use the generic
version of the Prelude's |mapM| function. This function allows us to |fmap| a
monadic action over a structure and transpose the result:

\begin{code}
mapM1 :: (Traversable f, AM m) => (a -> m b) -> f a -> m (f b)
\end{code}

%if False

\begin{code}
mapM1 = undefined
\end{code}

%endif

\end{subsection}

\begin{subsection}{Annotations}

In the previous we worked out some basic building blocks that can be useful
when working with container data types with the recursive point parametrized.
But why would it be useful to abstract away from the recursive points in the
first place? This section show how we can store additional information in the
recursive points using an annotated fixed point combinator.

First we introduce an new fixed point combinator that stores an annotation over
the container data type instead of a value of the data type itself. This type
level fixed point combinator is called |FixA|, the \emph{alpha} postfix
indicates it can store arbitrary stacks of annotations at the recursive
positions of the structure it contains.

\begin{code}
newtype FixA a f = In { out :: (a f) (FixA a f) }
\end{code}

\noindent
Note the kind of the annotation variable |a|, the annotation is applied over
the original container which has kind |* -> *|, because the annotation itself
needs to have the same kind the type variable |a| has kind |(* -> *) -> (* -> *)|.

Sometimes it is easier for functions to work with a structure with fully
annotated sub-structures. We create a type synonym |FixA1| that represents
this.

\begin{code}
type FixA1 a f = f (FixA a f)
\end{code}

\noindent
Sometimes it is easier for functions to work with a structure with annotated
fixed points without having the first |In| constructor around, directly
exposing the annotation value. We use the following type synonym -- which is
isomorphic to |FixA1| --  for this:

\begin{code}
type FixA2 a f = (a f) (FixA a f)
\end{code}

\noindent
We now introduce the identity annotation, called |Id|, that stores no
additional information but just encapsulates the underlying container type.

\begin{code}
newtype Id f a = Id { unId :: f a }
\end{code}

%if False
\begin{code}
  deriving Show
\end{code}
%endif

\noindent
The identity annotation can be used to get back the regular fixed point
combinator defined in the previous section by plugging it into a |FixA|.

\begin{code}
type Fix f = FixA Id f
\end{code}

\noindent
We also introduce a type synonym |Fix1| similar to the |FixA1| and |Fix2|
similar to |FixA2|, both aliases for the usage of identity annotations.

\begin{code}
type Fix1  f =      f   (Fix f)
type Fix2  f = (Id  f)  (Fix f)
\end{code}

\noindent
Using an annotated fixed point |FixA| or using a regular fixed point |Fix| both
require all sub-structures to be surrounded by an additional |In| constructor.
To make the usage of plain binary trees more easy we create a |Tree| type
synonym and two smart constructors: |leaf| and |branch|. Note these function
run in some context |m|.

\begin{code}
type Tree v = Fix (Tree_f v)
\end{code}

\begin{code}
leaf :: Tree v
leaf = In (Id Leaf)

branch :: v -> Tree v -> Tree v -> Tree v
branch v l r = In (Id (Branch v l r))
\end{code}

\noindent
The annotated fixed points can be used to store arbitrary pieces of data at the
recursive points of a recursive structure. For example, when you want a binary
tree annotated with the times at which some sub-tree was last modified you
could write something like this:

\begin{code}
data TimeAnn f a = TA LocalTime (f a)
type TimedTree v = FixA TimeAnn (Tree_f v)
\end{code}

\end{subsection}

\begin{subsection}{Annotation associated functionality}

In the previous section we showed how to store arbitrary pieces of information
at the recursive points of a data type. In this section we will show how to
associate functionality to these annotations. For every annotation type we will
describe how to obtain an annotation for a certain recursive point, we will
call this the |produce| function and how to get the recursive structure back
from the annotation, we call this the |query| function. The following type
signatures describe these two actions.

\begin{code}
type Produce  a f m  =      f (  FixA   a f)   -> m (     FixA   a f)

type Query    a f m  =           FixA   a f    -> m (f (  FixA   a f))
\end{code}

\noindent
As the type signature shows, a producer will take a structure with an annotated
substructure and introduces a new annotation for this structure. The function
might run in some -- possibly monadic -- context |m|, when this is required for
the annotation. The type signature for queries shows that it will take an
annotated structure and will use the annotation to give back the structure
itself. This functions can also run in the context |m|.

Two type classes are used to associate specific functionality to an annotation,
for queries this class is called |AnnQ|, for producers this class is called
|AnnM|. They both contains a single function with the type signature defined
above. The first parameter of the type class |a| is the annotation type, the
second parameter |f| is the structure to annotate, the third |m| is the context
it may run in.

\begin{code}
class (Traversable f, AM m) => AnnQ a f m where
  query :: Query a f m
\end{code}

\begin{code}
class (Traversable f, AM m) => AnnP a f m where
  produce :: Produce a f m
\end{code}

\noindent
Making an annotation type instance of this class means we can come up with an
annotation for a structure and can get back to the structure again. Note that
the |Traversable| and the |Monad| classes in the context are not strictly
necessary here. These super classes only help to prune the contexts when using
the |AnnQ| and |AnnM| classes. 

In all the examples that follow we assume that the notion of |Monad| in
a type context also assume the existinence of an |Applicative| instance.
Although this assumption is not strictly the case in Haskell it is valid in
theory and saves us some typing.

Now the instances for the identity annotation is very easy, we just unpack and
pack the annotation and strips off or introduces the |In| constructor.

\begin{code}
instance (Traversable f, AM m) => AnnQ Id f m where
  query = return . unId . out
\end{code}

\begin{code}
instance (Traversable f, AM m) => AnnP Id f m where
  produce = return . In . Id
\end{code}

\noindent
Although redundant in the general case, for possible optimizations we also
introduce a type class for modification of a sub-structure, called |AnnM|. The
|modify| function is used to apply a function over an annotated structure.
There is a default implementation available which is just the Kleisli
composition (denoted by |<=<|) of the query, the function, and the producer.

\begin{code}
type Modify   a f m  =   (  f (  FixA   a f)   -> m (f (  FixA   a f)))
                     ->  (       FixA   a f    -> m (     FixA   a f))
\end{code}

\begin{code}
class (AnnQ a f m, AnnP a f m) => AnnM a f m where
  modify :: Modify a f m
  modify f = produce <=< f <=< query
\end{code}

\noindent
For the identity we just use the default implementation.

\begin{code}
instance (Traversable f, AM m) => AnnM Id f m
\end{code}

\noindent
Now we have both defined annotated fixed points and a type class to associate
functionality with these annotations we can create two smart constructors to
simplify creating annotated trees manually.

\begin{code}
type TreeA a v = FixA a (Tree_f v)
\end{code}

\begin{code}
leafA :: AnnP a (Tree_f v) m => m (TreeA a v)
leafA = produce Leaf

branchA :: (AnnP a (Tree_f v) m) => v -> TreeA a v -> TreeA a v -> m (TreeA a v)
branchA v l r = produce (Branch v l r)
\end{code}

\end{subsection}

\begin{subsection}{Debug annotation}

To demonstrate the usage of generic traversals over annotated structures we
introduce the |Debug| annotation. In contrast to the identity annotation the
debug annotation does have associated functionality. We will make sure that the
debug annotation takes care of printing out every non-recursive piece it
traverses.

First we define the |Debug| data type that is just a |newtype| similar to the
identity annotation.

\begin{code}
newtype Debug f c = D { unD :: f c }
\end{code}

%if False
\begin{code}
  deriving Show
\end{code}
%endif

\noindent
Now we create a little helper function that can print out a predefined prefix
together with the some value and return that value again. Note that function
does not directly run in the |IO| monad, but in some monad |m| for which there
is a |MonadIO| instance, making it a bit more generally applicable.

\begin{code}
printer :: (MonadIO m, Show b) => String -> b -> m b
printer s f =
  do  liftIO (putStrLn (s ++ ": " ++ show f))
      return f
\end{code}

\noindent
Now we can supply the |AnnQ| instance for the |Debug| annotation by just
unpacking the constructor and printing out the structure we recurse.

\begin{code}
instance  (Traversable f, Applicative m, MonadIO m, Show (FixA1 Debug f))
      =>  AnnQ Debug f m where
  query = printer "query" . unD . out
\end{code}

\noindent
The same trick can be used for the dual instance |AnnM|.

\begin{code}
instance  (Traversable f, Applicative m, MonadIO m, Show (FixA1 Debug f))
      =>  AnnP Debug f m where
  produce = fmap (In . D) . printer "produce"
\end{code}

\noindent
For the |AnnM| we use the default implementation.

\begin{code}
instance  (Traversable f, Applicative m, MonadIO m, Show (FixA1 Debug f))
      =>  AnnM Debug f m
\end{code}

\noindent
In order to get these function to work properly we additionally need a |Show|
instance for our recursive structures.

\begin{code}
instance Show (FixA2 a f) => Show (FixA a f) where
  show f = "<" ++ show (out f) ++ ">"
\end{code}

\noindent
In the next chapters we will see how we can use the |Debug| annotation to print
out debug traces of generic traversals over annotated structures. Printing out
debug traces is just one example of what you can do with the annotation type
classes, in chapter TODO we will show how to use the same trick to store and
retrieve annotated structures on and from disk.

\end{subsection}

\end{section}

