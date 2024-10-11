module Nat where

import Prelude hiding (Num(..), Bool(..), (^), pred, min, (<), (<=), max, (>), (>=), (==), div, quot, rem)

import Bool

data Nat where
  O :: Nat
  S :: Nat -> Nat
  deriving (Eq, Show)

data ListNat where
  Empty :: ListNat
  Cons :: Nat -> ListNat -> ListNat
  deriving (Eq, Show)

o, so, sso, ssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso

(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

monus :: Nat -> Nat -> Nat
monus n O = n
monus n (S m) = pred (n - m)

(-) :: Nat -> Nat -> Nat
(-) = monus
-- n - O = n
-- n - S m = pred (n - m)

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
fib (S (S n)) = fib (S n) + fib n
fib n = n

min :: (Nat, Nat) -> Nat
min (_, O) = O
min (O, _) = O
min (S n, S m) = S (min (n, m))

(<) :: Nat -> Nat -> Bool
O < O = False
_ < O = False
O < _ = True
S n < S m = n < m

(<=) :: Nat -> Nat -> Bool
O <= O = True
_ <= O = False
O <= _ = True
S n <= S m = n <= m

max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, n) = n
max (S n, S m) = S (max (n, m))

(>) :: Nat -> Nat -> Bool
O > O = False
O > _ = False
_ > O = True
S n > S m = n > m

(>=) :: Nat -> Nat -> Bool
O >= O = True
O >= _ = False
_ >= O = True
S n >= S m = n >= m

(==) :: Nat -> Nat -> Bool
-- n == n = True
S n == S m = n == m
O == O = True
O == _ = False

div :: (Nat, Nat) -> (Nat, Nat)
div (_, O) = error "Division by zero isn't defined"
div (n, m) = (quot (n, m), rem (n, m))

quot :: (Nat, Nat) -> Nat
quot (n, O) = error "Division by zero isn't defined"
quot (O, n) = O
quot (n, S O) = n
-- quot (n, n) = S O
quot (n, m) = ifthenelse (m > n)  O  (S (quot (n - m, m)))
-- quot (n, m) = S (quot (n - m, m))

rem :: (Nat, Nat) -> Nat
rem (_, O) = error "Division by zero isn't defined"
rem (O, _) = O
rem (n, m) = n - (quot (n, m) * m)