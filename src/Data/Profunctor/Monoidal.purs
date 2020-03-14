module Data.Profunctor.Monoidal where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..), either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))

type Iso p a b = { fwd :: p a b, bwd :: p b a }

class Bifunctor t <= Associative t p
  where
  assoc :: forall a b c. Iso p (t a (t b c)) (t (t a b) c)

instance associativeCartesian :: Associative Tuple (->)
  where
  assoc =
    { fwd: \(a /\ b /\ c) -> (a /\ b) /\ c
    , bwd: \((a /\ b) /\ c) -> a /\ b /\ c
    }

instance associativeCocartesian :: Associative Either (->)
  where
  assoc =
    { fwd: either (Left <<< Left) (either (Left <<< Right) Right)
    , bwd: either (either Left (Right <<< Left)) (Right <<< Right)
    }

class Associative t p <= Tensor t i p
  where
  lunit :: forall a. Iso p (t i a) a
  runit :: forall a. Iso p (t a i) a

instance tensorCartesian :: Tensor Tuple Unit (->)
  where
  lunit = { fwd: snd, bwd: Tuple unit }
  runit = { fwd: fst, bwd: flip Tuple unit }

instance tensorCocartesian :: Tensor Either Void (->)
  where
  lunit = { fwd: either absurd identity, bwd: Right }
  runit = { fwd: either identity absurd, bwd: Left }

class (Associative l c, Associative r c, Profunctor p) <= Semigroupal c l r o p
  where
  pzip :: forall t u v w.
    c (o (p t u) (p v w)) (p (l t v) (r u w))

class Profunctor p <= Unital c l r o p
  where
  punit :: c o (p l r)

class (Semigroupal c l r o p, Unital c il ir io p) <= Monoidal c l il r ir o io p
