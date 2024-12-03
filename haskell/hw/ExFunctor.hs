module ExFunctor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const


instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

-- what about Either?
instance Funktor (Either e) where
    fmap f (Left e) = Left e
    fmap f (Right a) = Right (f a)

-- what about pairs?

-- what about functions?

-- what about Trees?

-- ...define Functor instances of as many * -> * things as you can think of!

