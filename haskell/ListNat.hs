module ListNat where

import Prelude(Eq(..), Show(..)) -- hiding (length)

import Nat

data ListNat where
  Nil :: ListNat
  Cons :: Nat -> ListNat -> ListNat
  deriving (Eq, Show)

lista, lista2 :: ListNat
lista = Cons ssso (Cons sso (Cons so (Cons O Nil)))
lista2 = Cons sso (Cons (sssso * sso) (Cons ssso Nil))
-- lista = Cons ssssso (Cons sso (Cons ssso (Cons sssso Nil)))
-- lista2 = Cons ssso (Cons sso (Cons so Nil))

length :: ListNat -> Nat
length Nil = O
length (Cons _ ns) = length ns

sum :: ListNat -> Nat
sum Nil = O
sum (Cons n ns) = n + sum ns

product :: ListNat -> Nat
product Nil = S O
product (Cons n ns) = n * product ns

addNat :: Nat -> ListNat -> ListNat
addNat _ Nil = Nil
addNat n (Cons m ms) = Cons (n + m) (addNat n ms)

mulNat :: Nat -> ListNat -> ListNat
mulNat _ Nil = Nil
mulNat n (Cons m ms) = Cons (n * m) (mulNat n ms)

expNat :: Nat -> ListNat -> ListNat
expNat _ Nil = Nil
expNat n (Cons m ms) = Cons (m ^ n) (expNat n ms)

powNat :: Nat -> ListNat -> ListNat
powNat _ Nil = Nil
powNat n (Cons m ms) = Cons (n ^ m) (powNat n ms)

pwAdd :: ListNat -> ListNat -> ListNat
pwAdd ns Nil = Nil
pwAdd (Cons n ns) (Cons m ms) = Cons (n + m) (pwAdd ns ms)

pwMul :: ListNat -> ListNat -> ListNat
pwMul ns Nil = Nil
pwMul (Cons n ns) (Cons m ms) = Cons (n * m) (pwMul ns ms)

pwExp :: ListNat -> ListNat -> ListNat
pwExp ns Nil = Nil
pwExp (Cons n ns) (Cons m ms) = Cons (n ^ m) (pwExp ns ms)

stretch :: Nat -> ListNat -> ListNat
stretch O _ = Nil
stretch (S O) ns = ns
-- todo recursive case

countdown :: Nat -> ListNat
countdown O = Cons O Nil
countdown (S n) = Cons (S n) (countdown n)