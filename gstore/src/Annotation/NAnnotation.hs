module Annotation.NAnnotation where

import Control.Applicative
import Control.Arrow
import Control.Category
import Generics.Types
import Prelude hiding ((.), id)

type Query   a h m = Kleisli m (NFixA  a h) (NFixA1 a h)
type Produce a h m = Kleisli m (NFixA1 a h) (NFixA  a h)
type Modify  a h m = Kleisli m (NFixA1 a h) (NFixA1 a h)
                  -> Kleisli m (NFixA  a h) (NFixA  a h)

class (Applicative m, Monad m) => AnnQ a m where
  query :: Query a h m

class (Applicative m, Monad m) => AnnP a m where
  produce :: Produce a h m

class (AnnQ a m, AnnP a m) => AnnM a m where
  modify :: Modify a h m
  modify f = produce . f . query

runQuery :: AnnQ a m => NFixA a h -> m (NFixA1 a h)
runQuery = runKleisli query

runProduce :: AnnP a m => NFixA1 a h -> m (NFixA a h)
runProduce = runKleisli produce

instance (Applicative m, Monad m) => AnnQ Id m where
  query = Kleisli (return . unId . nout)

instance (Applicative m, Monad m) => AnnP Id m where
  produce = Kleisli (return . NIn . Id)

instance (Applicative m, Monad m) => AnnM Id m

