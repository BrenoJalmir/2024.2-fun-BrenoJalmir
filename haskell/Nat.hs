module Nat where

import Prelude hiding (Num(..), (^), pred, min, max)

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

(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = (n * m) + n

(^) :: Nat -> Nat -> Nat
n ^ O = S O
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

-- min :: (Nat, Nat) -> Nat
-- min (n, O) = O
-- min (n, S m) = min (pred n, m)

-- max :: (Nat, Nat) -> Nat
-- max (n, O) = n
-- max (n, S m) = max (pred n, m)