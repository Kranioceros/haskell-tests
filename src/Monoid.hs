module Monoid where

data Bull =
  Fools
  | Twoo
  deriving(Eq, Show)

-- Bad monoid instance
instance Semigroup Bull where
  Fools <> x = x
  x <> Fools = x
  x <> y = y

instance Monoid Bull where
  mempty = Fools
  mappend = (<>)
