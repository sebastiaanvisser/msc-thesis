module Annotation.HAnnotation where

import Control.Applicative
import Control.Arrow
import Control.Category
import Generics.Types
import Prelude hiding ((.), id)

-- todo: put forall ix. in these synonyms?
type Produce a h ix m = Kleisli m (h (HFixA a h) ix)  (  (HFixA a h  ix))
type Query   a h ix m = Kleisli m    (HFixA a h  ix)  (h (HFixA a h) ix)
type Modify  a h ix m = Kleisli m (h (HFixA a h) ix)  (h (HFixA a h) ix)
                     -> Kleisli m (  (HFixA a h  ix)) (  (HFixA a h  ix))

class (Applicative m, Monad m) => HAnnQ a h ix m where
  query :: Query a h ix m

class (Applicative m, Monad m) => HAnnP a h ix m where
  produce :: Produce a h ix m

class (HAnnQ a h ix m, HAnnP a h ix m) => HAnnM a h ix m where
  modify :: Modify a h ix m
  modify f = produce . f . query

runQuery :: HAnnQ a h ix m => HFixA a h ix -> m (h (HFixA a h) ix)
runQuery = runKleisli query

runProduce :: HAnnP a h ix m => h (HFixA a h) ix -> m (HFixA a h ix)
runProduce = runKleisli produce

instance (Applicative m, Monad m) => HAnnQ HId h ix m where
  query = Kleisli (return . unHId . hout)

instance (Applicative m, Monad m) => HAnnP HId h ix m where
  produce = Kleisli (return . HIn . HId)

instance (Applicative m, Monad m) => HAnnM HId h ix m

