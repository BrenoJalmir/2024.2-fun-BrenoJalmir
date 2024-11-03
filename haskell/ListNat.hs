module ListNat where

import Prelude(Eq(..), Show(..)) -- hiding (length)

import Nat

data ListNat where
  Nil :: ListNat
  (:>) :: Nat -> ListNat -> ListNat
  deriving (Eq, Show)

lista, lista2 :: ListNat
-- lista = ssso :> (sso :> (so :> (O :> Nil)))
lista = sso :> (so :> Nil)
lista2 = sso :> ((sssso <*> sso) :> (ssso :> Nil))
-- lista = ssssso :> (sso :> (ssso :> (sssso Nil)))
-- lista2 = ssso :> (sso :> (so Nil))

head :: ListNat -> Nat
head (n :> _) = n

tails :: ListNat -> ListNat
tails (_ :> ns) = ns

length :: ListNat -> Nat
length Nil = O
length (_ :> ns) = length ns

sum :: ListNat -> Nat
sum Nil = O
sum (n :> ns) = n <+> sum ns

product :: ListNat -> Nat
product Nil = S O
product (n :> ns) = n <*> product ns

addNat :: Nat -> ListNat -> ListNat
addNat _ Nil = Nil
addNat n (m :> ms) = (n <+> m) :> addNat n ms

mulNat :: Nat -> ListNat -> ListNat
mulNat _ Nil = Nil
mulNat n (m :> ms) = (n <*> m) :> mulNat n ms

expNat :: Nat -> ListNat -> ListNat
expNat _ Nil = Nil
expNat n (m :> ms) = (m <^> n) :> expNat n ms

powNat :: Nat -> ListNat -> ListNat
powNat _ Nil = Nil
powNat n (m :> ms) = (n <^> m) :> powNat n ms

pwAdd :: ListNat -> ListNat -> ListNat
pwAdd ns Nil = Nil
pwAdd (n :> ns) (m :> ms) = (n <+> m) :> pwAdd ns ms

pwMul :: ListNat -> ListNat -> ListNat
pwMul ns Nil = Nil
pwMul (n :> ns) (m :> ms) = (n <*> m) :> pwMul ns ms

pwExp :: ListNat -> ListNat -> ListNat
pwExp ns Nil = Nil
pwExp (n :> ns) (m :> ms) = (n <^> m) :> pwExp ns ms

repeat :: Nat -> Nat -> ListNat -> ListNat
repeat (S O) x ms = x :> ms
repeat (S n) x ms = repeat n x (x :> ms)

stretch :: Nat -> ListNat -> ListNat
stretch (S O) ns = ns
stretch (S n) (m :> Nil) = repeat n m (m :> Nil)
stretch (S n) (m :> ms) = repeat (S n) m (stretch (S n) ms)

countdown :: Nat -> ListNat
countdown O = O:>Nil
countdown (S n) = S n:>countdown n