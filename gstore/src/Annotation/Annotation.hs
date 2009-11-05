module Annotation.Annotation where

import Control.Applicative
import Control.Arrow
import Control.Category
import Generics.Types
import Prelude hiding ((.), id)

type Produce a f m = Kleisli m (f (FixA a f)) (  (FixA a f))
type Query   a f m = Kleisli m (  (FixA a f)) (f (FixA a f))
type Modify  a f m = Kleisli m (f (FixA a f)) (f (FixA a f))
                  -> Kleisli m (  (FixA a f)) (  (FixA a f))

class (Applicative m, Monad m) => AnnQ a f m where
  query :: Query a f m

class (Applicative m, Monad m) => AnnP a f m where
  produce :: Produce a f m

class (AnnQ a f m, AnnP a f m) => AnnM a f m where
  modify :: Modify a f m
  modify f = produce . f . query

runQuery :: AnnQ a f m => FixA a f -> m (f (FixA a f))
runQuery = runKleisli query

runProduce :: AnnP a f m => f (FixA a f) -> m (FixA a f)
runProduce = runKleisli produce

instance (Applicative m, Monad m) => AnnQ Id f m where
  query = Kleisli (return . unId . out)

instance (Applicative m, Monad m) => AnnP Id f m where
  produce = Kleisli (return . In . Id)

instance (Applicative m, Monad m) => AnnM Id f m

