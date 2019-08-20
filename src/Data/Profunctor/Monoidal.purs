module Data.Profunctor.Monoidal where

import Data.Profunctor (class Profunctor)

class Profunctor p <= Semigroupal s t p
  where
  zip ::
    forall a b c d.
    t (p a b) (p c d) -> p (s a c) (s b d)

class Semigroupal s t p <= Monoidal s t i j p
  | s p -> i, t p -> j, i p -> s, j p -> t
  where
  unit :: j -> p i i
