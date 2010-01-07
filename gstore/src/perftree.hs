

-- ******************************************************** TEST **********************




data PTree a = PLeaf a | PNode (PTree (a,a))

data HPTree f a = HPLeaf a | HPNode (f (a,a))

-- type PTree a = HFixA HPTree a

instance HFunctor HPTree where
  hfmap _ (HPLeaf a) = HPLeaf a
  hfmap f (HPNode a) = HPNode (f a)

hfoldPTree
  :: (forall a. a       -> f a)
  -> (forall a. f (a,a) -> f a)
  -> PTree b -> f b
hfoldPTree f _ (PLeaf x)  = f x
hfoldPTree f g (PNode xs) = g (hfoldPTree f g xs)


-- type Blaat = Ran (K Int) (K Int)

myPTree :: PTree Int
myPTree =
  (PNode . PNode . PNode . PLeaf)
  (((1, 2), (3, 4)), ((5, 6), (7, 8)))

psum :: PTree Int -> Int
psum = aux id
  where aux :: (a -> Int) -> PTree a -> Int
        aux f (PLeaf x)  = f x
        aux f (PNode xs) = aux (\(a, b) -> f a + f b) xs

bliep :: [Int]
bliep = hfoldPTree x y myPTree
  where x = (:[])
        y = Prelude.concatMap (\(a, b) -> [b, a])

alg :: HPTree (Ran String String) a -> Ran String String a
alg (HPLeaf a) = Ran (\s -> s a)
alg (HPNode r) = Ran (\s -> unRan r (\(a, b) -> "(" ++ s a ++ ", " ++ s b ++ ")"))

