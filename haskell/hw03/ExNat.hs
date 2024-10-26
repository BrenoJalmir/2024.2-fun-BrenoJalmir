{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    O == O = True
    (S n) == (S m) = n == m
    _ == _ = False

instance Ord Nat where

    O <= _ = True
    _ <= O = False
    (S n) <= (S m) = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = O
    min _ O = O
    min (S n) (S m) = S (min n m)

    max n O = n
    max O n = n
    max (S n) (S m) = S (max n m)


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n 

-- o, so, sso, ssso, sssso, ssssso :: Nat
-- o = O
-- so = S o
-- sso = S so
-- ssso = S sso
-- sssso = S ssso
-- ssssso = S sssso

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> (S m) = S (n <+> m)

infixl 1 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
n <-> O = n
n <-> (S m) = pred (n <-> m)

infixl 1 <->

-- multiplication
(<*>) :: Nat -> Nat -> Nat
n <*> O = O
n <*> (S m) = n <*> m <+> n

infixl 2 <*>

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
n <^> O = S O
n <^> (S O) = n
n <^> (S m) = n <^> m <*> n

infixr 3 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "Division by zero"
n </> m = 
  if max n m == n
  then S ((n <-> m) </> m)
  else O

infixl 2 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = error "Division by zero"
n <%> m = 
  if min n m == n
  then n
  -- else n - n </> m <*> m 
  else (n <-> m) <%> m

infix 3 <%>

-- divides
(<|>) :: Nat -> Nat -> Bool
_ <|> O = error "Division by zero"
n <|> m = isZero (n <%> m)

divides = (<|>)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n m = 
  if max n m == n
  then n - m
  else m - n

(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n <*> factorial n

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
  else S (lo (S (S n)) (m </> S (S n)))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat n
  | n == 0    = O
  | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat n 
  |n == O = 0
  |otherwise = fromNat (n - 1) + 1


-- Voilá: we can now easily make Nat an instance of Num.
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

