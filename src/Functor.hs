module Functor(Listona(..))where

data Listona a = ListonaVacia | Listona a (Listona a)
  deriving (Eq)

instance Functor Listona where
  fmap _ (ListonaVacia) = ListonaVacia
  fmap f (Listona x ls) = Listona (f x) (fmap f ls)

instance Show a => Show (Listona a) where
  show ListonaVacia = "{}"
  show (Listona x ls) =
    let show_rest ListonaVacia = "}"
        show_rest (Listona x ls) = ", " ++ show x ++ show_rest ls
    in "{" ++ show x ++ show_rest ls
