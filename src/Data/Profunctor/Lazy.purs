module Data.Profunctor.Lazy where

import Data.Unit (Unit)

class Lazy2 p where
  defer2 :: forall x y. (Unit -> p x y) -> p x y
