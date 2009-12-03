{-# LANGUAGE GADTs #-}

module TreeDelete where

import Control.Applicative

data Tree = Node Tree Int Tree | Leaf
  deriving Show

data TreeAlg r where
  TA      :: (r -> Int -> r -> r, r) -> TreeAlg r
  Project :: (r -> s) -> TreeAlg r -> TreeAlg s


(<+>) :: TreeAlg r -> TreeAlg s -> TreeAlg (r,s)
Project f tf <+> Project g tg = Project (\ (x,y) -> (f x,g y)) (tf <+> tg)
Project f tf <+> tg           = Project f tf  <+> Project id tg
tf           <+> Project g tg = Project id tf <+> Project g tg
TA (rn,rl) <+> TA (sn,sl)     = TA (\ (rl',sl') x (rr',sr') ->
                                      (rn rl' x rr', sn sl' x sr')
                                      ,(rl,sl))


lift :: (r -> TreeAlg s) -> TreeAlg (r -> s)
lift = undefined

f >>= g = lift g <*> f

instance Functor TreeAlg where
  fmap = Project

instance Applicative TreeAlg where
  pure  x   = TA (\ _ _ _ -> x, x)
  p1 <*> p2 = Project (uncurry ($)) (p1 <+> p2)


foldTree :: TreeAlg r -> Tree -> r
foldTree (Project f ta) t =
  f (foldTree ta t)
foldTree alg@(TA (node,leaf)) t =
  case t of
    Node l x r -> node (foldTree alg l) x (foldTree alg r)
    Leaf       -> leaf

-- repmin:

minAlg :: TreeAlg Int
minAlg = TA (\ ml x mr -> minimum [ml, x, mr],maxBound)

repAlg :: TreeAlg (Int -> Tree)
repAlg = TA (\ rl x rr a -> Node (rl a) a (rr a), const Leaf)

repMinAlg :: TreeAlg Tree
repMinAlg = repAlg <*> minAlg

shared = (\ f g x -> (f x, g x)) <$> repAlg <*> repAlg <*> minAlg
unshared = let m = minAlg in (repAlg <*> m) <+> (repAlg <*> m)


-- delete:

delete :: Int -> Tree -> Tree
delete z Leaf = Leaf
delete z (Node l x r) =
  case compare z x of
    LT -> Node (delete z l) x r
    GT -> Node l x (delete z r)
    EQ -> join l r

join :: Tree -> Tree -> Tree
join Leaf           r = r
join (Node l' x r') r = split r' l' x (\ ll xx -> Node ll xx r)

split :: Tree -> Tree -> Int -> (Tree -> Int -> Tree) -> Tree
split Leaf            l x c = c l x
split (Node l_ x_ r_) l x c = split r_ l_ x_ (\ r' y -> c (Node l x r') y)



a :: Int -> TreeAlg (Tree, Tree, Tree -> Tree, Tree -> Int -> (Tree -> Int -> Tree) -> Tree)
a z = TA
      (\ (subLeft,deletedLeft,joinedLeft,splitLeft) val (subRight,deletedRight,jr,splitRight) ->
         ( Node subLeft val subRight
         , deleteNode val deletedLeft deletedRight joinedLeft subLeft subRight
         , joinNode val subLeft subRight splitRight
         , splitNode val subLeft subRight splitRight
         ),
       (Leaf, Leaf, \r -> r, \ subLeft val c -> c subLeft val))
  where
    deleteNode val deletedLeft deletedRight joinedLeft subLeft subRight =
      case compare z val of
        LT -> Node deletedLeft val subRight
        GT -> Node subLeft val deletedRight
        EQ -> joinedLeft subRight

    joinNode val subLeft subRight splitRight =
      \ rx -> splitRight subLeft val (\ subLeft' val -> Node subLeft' val rx)

    splitNode val subLeft subRight splitRight =
      \ lx xx c -> splitRight subLeft val (\ r' y -> c (Node lx xx r') y)

delete' :: Int -> Tree -> Tree
delete' z = (\ (_,x,_,_) -> x) . foldTree (a z)

leaf :: Int -> Tree
leaf x = Node Leaf x Leaf

testTree :: Tree
testTree =
  Node
    (Node
       (Node
          (leaf 0)
          1
          (Node (leaf 2) 3 Leaf))
       5
       (Node (leaf 6) 7 (leaf 8)))
    9
    (Node (leaf 10) 11 (leaf 12))
