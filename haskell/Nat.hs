{-# LANGUAGE GADTs #-}

module Nat where

import Prelude hiding ((<*>), (<|>), quot, rem, gcd, lcm, odd)

data Nat where
  O :: Nat
  S :: Nat -> Nat

instance Show Nat where
  -- Derived Show below
  -- show O = "O"
  -- show (S n) = "S (" ++ show n ++ ")"

  show O = "O"
  show (S n) = "S" ++ show n

instance Eq Nat where
  -- (===) :: Nat -> Nat -> Bool
  -- O === O = True
  -- S n === S m = n === m
  -- _ === _ = False

  O == O = True
  S n == S m = n == m
  _ == _ = False

instance Ord Nat where

  O <= _ = True
  _ <= O = False
  S n <= S m = n <= m

  -- (<) :: Nat -> Nat -> Bool
  -- _ < O = False
  -- O < _ = True
  -- S n < S m = n < m

  -- (<=) :: Nat -> Nat -> Bool
  -- O <= _ = True
  -- _ <= O = False
  -- S n <= S m = n <= m
  
  -- min :: Nat -> Nat -> Nat
  min _ O = O
  min O _ = O
  min (S n) (S m) = S (min n m)

  -- (>) :: Nat -> Nat -> Bool
  -- O > _ = False
  -- _ > O = True
  -- S n > S m = n > m

  -- (>=) :: Nat -> Nat -> Bool
  -- _ >= O = True
  -- O >= _ = False
  -- S n >= S m = n >= m

  -- max :: (Nat, Nat) -> Nat
  max n O = n
  max O n = n
  max (S n) (S m) = S (max n m)

o, so, sso, ssso, sssso, ssssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso
sssso = S ssso
ssssso = S sssso

-- fst :: (a, a) -> a
-- fst (a, b) = a

-- snd :: (a, a) -> a
-- snd (a, b) = b

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S (n <+> m)

(<->) :: Nat -> Nat -> Nat
n <-> O = n
n <-> (S m) = predNat (n <-> m)

(<*>) :: Nat -> Nat -> Nat
_ <*> O = O
n <*> S m = n <+> (n <*> m)

(<^>) :: Nat -> Nat -> Nat
_ <^> O = S O
n <^> S m = (n <^> m) <*> n

predNat :: Nat -> Nat
predNat O = O
predNat (S n) = n

fact :: Nat -> Nat
fact (S n) = S n * fact n
fact _ = S O

fib :: Nat -> Nat
fib (S (S n)) = fib (S n) + fib n
fib n = n

-- divNat :: (Nat, Nat) -> (Nat, Nat)
-- divNat (_, O) = error "division by O"
-- divNat (O, _) = (O, O)
-- divNat (n, S O) = (n, O)
-- divNat (n, m) = 
--   ifthenelse (m > n)
--     (O, n)
--     (let (q', r') = divNat (n - m, m)
--       in (S q', r'))

-- quot :: (Nat, Nat) -> Nat
-- quot (n, m) = fst (divNat (n, m))

-- rem :: (Nat, Nat) -> Nat
-- rem (n, m) = snd (divNat (n, m))

-- quotient
quot :: Nat -> Nat -> Nat
quot _ O = error "Division by zero"
quot n m = 
  if max n m == n
  then S ((n <-> m) `quot` m)
  else O

-- remainder
rem :: Nat -> Nat -> Nat
rem _ O = error "Division by zero"
rem n m = 
  if min n m == n && n /= m
    then n
    else rem (n <-> m) m

-- quot' :: (Nat, Nat) -> Nat
-- quot' (n, O) = error "Division by zero isn't defined"
-- quot' (O, n) = O
-- quot' (n, S O) = n
-- quot' (n, m) = 
--   ifthenelse (m > n)
--   O
--   (S (quot' (n - m, m)))

-- rem' :: (Nat, Nat) -> Nat
-- rem' (_, O) = error "Division by zero isn't defined"
-- rem' (O, _) = O
-- rem' (n, m) = n - (quot' (n, m) * m)

-- div' :: (Nat, Nat) -> (Nat, Nat)
-- -- div' (_, O) = error "Division by zero isn't defined"
-- div' (n, m) = (quot' (n, m), rem' (n, m))

gcd :: Nat -> Nat -> Nat
gcd n O = n
gcd n (S O) = S O
gcd n m = 
  if m > n
  then gcd m n
  else gcd m (rem n m)

lcm :: Nat -> Nat -> Nat
lcm _ O = O
lcm _ (S O) = S O
lcm n m = 
  if rem n m == O
  then max n m
  else quot (n * m) (gcd n m)

(<|>) :: Nat -> Nat -> Bool
_ <|> O = error "Division by zero"
n <|> m = isZero (rem n m)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n m = 
  if max n m == n
  then n <-> m
  else m <-> n

(|-|) = absDiff

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg n = S O
-- sg _ = error "Negative number in Nat"

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo (S (S n)) m = 
  if max (S (S n)) m == S (S n) && S (S n) /= m
  then O
  else S (lo (S (S n)) (m `quot` S (S n)))

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat i = S (toNat (i - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat n = succ (fromNat (n - 1))

instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0     = error "Negative Nat"
    | x == 0    = toNat x
    | otherwise = toNat x

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n


