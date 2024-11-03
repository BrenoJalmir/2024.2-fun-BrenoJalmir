{-# LANGUAGE GADTs #-}

module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

import ExNat

-- lista, lista2 :: [Nat]
-- lista = [sso, so]
-- lista2 = [sso, sssso * sso, ssso]

head :: [a] -> a
head [] = error "head of Nil"
head (n : _) = n

tail :: [a] -> [a]
tail [] = error "tail of Nil"
tail (_ : ns) = ns

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (n:ns) = succ (length ns)

sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + sum ns

product :: Num a => [a] -> a
product [] = 1
product (n:ns) = n * product ns

reverse :: [a] -> [a]
reverse [n] = [n]
reverse (n:ns) = [n] ++ reverse ns -- don't use vs code sugestion, it doesn't work

(++) :: [a] -> [a] -> [a]
ns ++ [] = ns
(n:ns) ++ (m:ms) = m:(n:ns) ++ ms

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc n [] = [n]
snoc n (m:ms) = m:snoc n ms
-- snoc n ms = reverse (n : reverse ms)

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm??)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
-- maximum :: Ord a => [a] -> a

-- take
-- drop

-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- Aux's func's to palindrome
normalizeString :: String -> String
normalizeString ss = [C.toLower s | s <- ss, C.isLetter s]

pwStringChecker :: String -> String -> Bool
pwStringChecker "" "" = True
pwStringChecker "" _ = False
pwStringChecker _ "" = False
pwStringChecker (s:ss) (z:zs) = s == z && pwStringChecker ss zs

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome "" = True
palindrome cs = pwStringChecker (normalizeString cs) (reverse (normalizeString cs))

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

