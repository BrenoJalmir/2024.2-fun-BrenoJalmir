module Nat where

import Prelude hiding (Num(..), (^), pred, min, (<), max, (>), div, quot, rem)

data Nat where
  O :: Nat
  S :: Nat -> Nat
  deriving (Eq, Show)

o, so, sso, ssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso

(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

(-) :: Nat -> Nat -> Nat
n - O = n
n - S m = pred (n - m)

(*) :: Nat -> Nat -> Nat
_ * O = O
n * S m = (n * m) + n

(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ S m = (n ^ m) * n

double = (*) sso

pred :: Nat -> Nat
pred O = O
pred (S n) = n

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib (S (S x)) = fib (S x) + fib x
fib _ = S O

min :: (Nat, Nat) -> Nat
min (_, O) = O
min (O, _) = O
min (S n, S m) = S (min (n, m))

(<) :: Nat -> Nat -> Bool
O < O = False
_ < O = False
O < n = True
S n < S m = n < m

max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, n) = n
max (S n, S m) = S (max (n, m))

(>) :: Nat -> Nat -> Bool
O > O = False
O > _ = False
_ > O = True
S n > S m = n > m

-- isZero :: Nat -> Bool
-- isZero O = True
-- isZero (S _) = False

-- div :: (Nat, Nat) -> (Nat, Nat)
-- div (O, _) = (O, O)
-- div (_, O) = error "Division by zero isn't defined"
-- div (n, S O) = (n, O)
-- div (n, S (S m)) = (O, S (S m))


-- quot :: (Nat, Nat) -> Nat
-- quot (n, O) = error "Division by zero isn't defined"
-- quot (n, S O) = n
-- quot (n, S (S m)) = 

-- rem :: (Nat, Nat) -> Nat
-- rem (_, O) = error "Division by zero isn't defined"
-- rem (n, S O) = O
-- rem (n, S (S m)) = 