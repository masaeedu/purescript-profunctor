module Data.Profunctor.Monoidal where

import Control.Category.Tensor (class Associative, class Tensor)
import Data.Profunctor (class Profunctor)

import Data.Tuple (Tuple, curry)
import Data.Tuple.Nested (type (/\))

import Data.Either (Either)
import Data.Either.Nested (type (\/))

class (Associative l c, Associative r c, Associative o c, Profunctor p) <= Semigroupal c l r o p
  where
  pzip :: forall d e f g.
    c (o (p d e) (p f g)) (p (l d f) (r e g))

mux :: forall p a b c d. Semigroupal (->) Tuple Tuple Tuple p => p a b -> p c d -> p (a /\ c) (b /\ d)
mux = curry pzip

infixr 5 mux as &&

demux :: forall p a b c d. Semigroupal (->) Either Either Tuple p => p a b -> p c d -> p (a \/ c) (b \/ d)
demux = curry pzip

infixr 4 demux as ||

switch :: forall p a b c d. Semigroupal (->) Tuple Either Tuple p => p a b -> p c d -> p (a /\ c) (b \/ d)
switch = curry pzip

infixr 5 switch as &|

splice :: forall p a b c d. Semigroupal (->) Either Tuple Tuple p => p a b -> p c d -> p (a \/ c) (b /\ d)
splice = curry pzip

infixr 5 splice as |&

class Profunctor p <= Unital c l r o p
  where
  punit :: c o (p l r)

class (Tensor l il c, Tensor r ir c, Tensor o io c, Semigroupal c l r o p, Unital c il ir io p) <= Monoidal c l il r ir o io p
