module ListNat where

import Prelude hiding (length)

import Nat

data ListNat where
  Empty :: ListNat
  Cons :: Nat -> ListNat -> ListNat
  deriving (Eq, Show)

length :: ListNat -> Nat
length Empty = O
length (Cons _ ns) = length ns
