module Num where

class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (^) :: a -> a -> a
  (-) :: a -> a -> a
  -- O :: a