module Data.Profunctor.Monoidal where

import Control.Category.Tensor (class Associative)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple, curry)

class (Associative l c, Associative r c, Associative o c, Profunctor p) <= Semigroupal c l r o p
  where
  pzip :: forall d e f g.
    c (o (p d e) (p f g)) (p (l d f) (r e g))

pzipInfix :: forall l r p a b c d. Semigroupal (->) l r Tuple p => p a b -> p c d -> p (l a c) (r b d)
pzipInfix = curry pzip

infixr 5 pzipInfix as &&&

class Profunctor p <= Unital c l r o p
  where
  punit :: c o (p l r)

class (Semigroupal c l r o p, Unital c il ir io p) <= Monoidal c l il r ir o io p
