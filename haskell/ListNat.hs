module ListNat where

import Prelude(Eq(..), Show(..)) -- hiding (length)

import Nat

data ListNat where
  Empty :: ListNat
  Cons :: Nat -> ListNat -> ListNat
  deriving (Eq, Show)

lista, lista2 :: ListNat
lista = Cons ssso (Cons sso (Cons so (Cons O Empty)))
lista2 = Cons ssso (Cons sso (Cons so Empty))

length :: ListNat -> Nat
length Empty = O
length (Cons _ ns) = length ns

sum :: ListNat -> Nat
sum Empty = O
sum (Cons n ns) = n + sum ns

product :: ListNat -> Nat
product Empty = S O
product (Cons n ns) = n * product ns

addNat :: Nat -> ListNat -> ListNat
addNat _ Empty = Empty
addNat n (Cons m ms) = Cons (n + m) (addNat n ms)

mulNat :: Nat -> ListNat -> ListNat
mulNat _ Empty = Empty
mulNat n (Cons m ms) = Cons (n * m) (mulNat n ms)

expNat :: Nat -> ListNat -> ListNat
expNat _ Empty = Empty
expNat n (Cons m ms) = Cons (m ^ n) (expNat n ms)

powNat :: Nat -> ListNat -> ListNat
powNat _ Empty = Empty
powNat n (Cons m ms) = Cons (n ^ m) (powNat n ms)