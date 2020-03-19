module Data.Profunctor.Bimodule where

import Control.Category.Tensor (class Associative)
import Data.Profunctor (class Profunctor)

class (Associative t (->), Profunctor p) <= LeftModule t p
  where
  lstrength :: forall a b x. p a b -> p (t a x) (t b x)

class (Associative t (->), Profunctor p) <= RightModule t p
  where
  rstrength :: forall a b x. p a b -> p (t x a) (t x b)

class (LeftModule l p, RightModule r p) <= Bimodule l r p
