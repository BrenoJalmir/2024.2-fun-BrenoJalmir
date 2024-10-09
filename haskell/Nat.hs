module Nat where

import Prelude hiding (Num(..), (^), pred)

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
