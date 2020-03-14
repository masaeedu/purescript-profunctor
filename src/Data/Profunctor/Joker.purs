module Data.Profunctor.Joker where

import Prelude

import Control.MonadPlus (class MonadZero)
import Data.Either (Either(..), either)
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Data.Newtype (class Newtype, un)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Monoidal (class Monoidal, class Semigroupal, class Unital)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

-- | Makes a trivial `Profunctor` for a covariant `Functor`.
newtype Joker f a b = Joker (f b)

derive instance newtypeJoker :: Newtype (Joker f a b) _
derive newtype instance eqJoker :: Eq (f b) => Eq (Joker f a b)
derive newtype instance ordJoker :: Ord (f b) => Ord (Joker f a b)

instance showJoker :: Show (f b) => Show (Joker f a b) where
  show (Joker x) = "(Joker " <> show x <> ")"

instance functorJoker :: Functor f => Functor (Joker f a) where
  map f (Joker a) = Joker (map f a)

instance profunctorJoker :: Functor f => Profunctor (Joker f) where
  dimap f g (Joker a) = Joker (map g a)

instance clownJoker :: Functor f => Choice (Joker f) where
  left  (Joker f) = Joker $ map Left f
  right (Joker f) = Joker $ map Right f

instance applyJoker :: Apply f => Apply (Joker f a) where
  apply (Joker f) (Joker g) = Joker $ apply f g

instance applicativeJoker :: Applicative f => Applicative (Joker f a) where
  pure = Joker <<< pure

instance bindJoker :: Bind f => Bind (Joker f a) where
  bind (Joker ma) amb = Joker $ ma >>= (amb >>> un Joker)

instance monadJoker :: Monad m => Monad (Joker m a)

instance cochoiceJoker :: MonadZero f => Cochoice (Joker f)
  where
  unleft  (Joker fa) = Joker $ fa >>= either pure (const empty)
  unright (Joker fb) = Joker $ fb >>= either (const empty) pure

instance ttSemigroupalJoker :: Apply f => Semigroupal (->) Tuple Tuple Tuple (Joker f) where
  pzip (Joker f /\ Joker g) = Joker $ (/\) <$> f <*> g

instance ttUnitalJoker :: Applicative f => Unital (->) Unit Unit Unit (Joker f) where
  punit = Joker <<< pure

instance ttMonoidalJoker :: Applicative f => Monoidal (->) Tuple Unit Tuple Unit Tuple Unit (Joker f)

instance etSemigroupalJoker :: Alt f => Semigroupal (->) Either Either Tuple (Joker f) where
  pzip (Joker f /\ Joker g) = Joker $ (Left <$> f) <|> (Right <$> g)

instance etUnitalJoker :: Alternative f => Unital (->) Void Void Unit (Joker f) where
  punit = const $ Joker $ empty

instance etMonoidalJoker :: Alternative f => Monoidal (->) Either Void Either Void Tuple Unit (Joker f)

instance eeSemigroupalJoker :: Functor f => Semigroupal (->) Either Either Either (Joker f) where
  pzip (Left (Joker f)) = Joker $ map Left f
  pzip (Right (Joker f)) = Joker $ map Right f

instance eeUnitalJoker :: Functor f => Unital (->) Void Void Void (Joker f) where
  punit = absurd

hoistJoker :: forall f g a b. (f ~> g) -> Joker f a b -> Joker g a b
hoistJoker f (Joker a) = Joker (f a)
