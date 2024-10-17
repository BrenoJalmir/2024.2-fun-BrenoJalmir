{-# LANGUAGE GADTs #-}

module Nat where

import Prelude(Eq(..), Show(..), (++), undefined)
-- import Prelude hiding (Num(..), Bool(..), (^), pred, min, (<), (<=), max, (>), (>=), (==), div, quot, rem)

import Bool

data Nat where
  O :: Nat
  S :: Nat -> Nat
  deriving (Eq)

instance Show Nat where
  show O = "O"
  show (S n) = "S" ++ show n

  -- Derived Show below
  -- show O = "O"
  -- show (S n) = "S (" ++ show n ++ ")"

-- instance Eq Nat where
--   O == O = True
--   S n == S m = n == m
--   _ == _ = False

-- instance Num Nat where
--   (+) = plus
--   (-) = monus
--   (*) = times
--   (^) = power

fst :: (a, a) -> a
fst (a, b) = a

snd :: (a, a) -> a
snd (a, b) = b

o, so, sso, ssso, sssso, ssssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso
sssso = S ssso
ssssso = S sssso

-- plus :: Num Nat => Nat -> Nat -> Nat
-- plus n O = n
-- plus n (S m) = S (plus n m)
(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

monus :: Nat -> Nat -> Nat
monus n O = n
monus n (S m) = pred (monus n m)

(-) :: Nat -> Nat -> Nat
(-) = monus
-- n - O = n
-- n - S m = pred (n - m)

-- times :: Nat -> Nat -> Nat
-- times _ O = O
-- times n (S m) = times n m + n
(*) :: Nat -> Nat -> Nat
_ * O = O
n * S m = (n * m) + n

-- power :: Nat -> Nat -> Nat
-- power _ O = S O
-- power n (S m) = power n m * n
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ S m = (n ^ m) * n

double = (*) sso

pred :: Nat -> Nat
pred O = O
pred (S n) = n

fact :: Nat -> Nat
fact (S n) = S n * fact n
fact _ = S O

fib :: Nat -> Nat
fib (S (S n)) = fib (S n) + fib n
fib n = n

min :: (Nat, Nat) -> Nat
min (_, O) = O
min (O, _) = O
min (S n, S m) = S (min (n, m))

(<) :: Nat -> Nat -> Bool
_ < O = False
O < _ = True
S n < S m = n < m

(<=) :: Nat -> Nat -> Bool
O <= _ = True
_ <= O = False
S n <= S m = n <= m

max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, n) = n
max (S n, S m) = S (max (n, m))

(>) :: Nat -> Nat -> Bool
O > _ = False
_ > O = True
S n > S m = n > m

(>=) :: Nat -> Nat -> Bool
_ >= O = True
O >= _ = False
S n >= S m = n >= m

(===) :: Nat -> Nat -> Bool
O === O = True
S n === S m = n === m
_ === _ = False

div :: (Nat, Nat) -> (Nat, Nat)
div (_, O) = undefined
div (O, _) = (O, O)
div (n, S O) = (n, O)
div (n, m) = 
  ifthenelse (m > n)
    (O, n)
    (let (q', r') = div (n - m, m)
      in (S q', r'))

quot :: (Nat, Nat) -> Nat
quot (n, m) = fst (div (n, m))

rem :: (Nat, Nat) -> Nat
rem (n, m) = snd (div (n, m))

-- quot' :: (Nat, Nat) -> Nat
-- quot' (n, O) = undefined -- error "Division by zero isn't defined"
-- quot' (O, n) = O
-- quot' (n, S O) = n
-- quot' (n, m) = 
--   ifthenelse (m > n)
--   O
--   (S (quot' (n - m, m)))

-- rem' :: (Nat, Nat) -> Nat
-- rem' (_, O) = undefined -- error "Division by zero isn't defined"
-- rem' (O, _) = O
-- rem' (n, m) = n - (quot' (n, m) * m)

-- div' :: (Nat, Nat) -> (Nat, Nat)
-- -- div' (_, O) = undefined -- error "Division by zero isn't defined"
-- div' (n, m) = (quot' (n, m), rem' (n, m))

gcd :: (Nat, Nat) -> Nat
gcd (n, O) = n
gcd (n, S O) = S O
gcd (n, m) = 
  ifthenelse (m > n)
  (gcd (m, n))
  (gcd (m, rem (n, m)))

lcm :: (Nat, Nat) -> Nat
lcm (_, O) = O
lcm (_, S O) = S O
lcm (n, m) = 
  ifthenelse (rem (n, m) === O)
  (max (n, m))
  (quot (n * m, gcd (n, m)))