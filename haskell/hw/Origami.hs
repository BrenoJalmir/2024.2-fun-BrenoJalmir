module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , scanl, scanr
    , sum , product
    , concat
    , any , all
    , and , or
    , minimum, maximum
    , length
    , filter
    , map
    , reverse
    , takeWhile , dropWhile
    )

import qualified Prelude as P
import qualified Data.Maybe as M

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (a:as) =  f a (foldr f acc as)

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (a:as) = foldl f (f acc a) as

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [a, a'] = a `f` a'
foldr1 f (a:a':as) = a `f` foldr1 f (a':as)

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f [a, a'] = a `f` a'
foldl1 f (a:a':as) = foldl1 f (a':as) `f` a


--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ acc [] = [acc]
scanl f acc (a:as) = acc : scanl f (f acc a) as

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ acc [] = [acc]
scanr f acc (a:as) = scanr f (f a acc) as ++ [acc]

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum = foldl (+) 0

product :: Num a => [a] -> a
product = foldl (*) 1

concat :: [[a]] -> [a]
concat = foldl (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = foldl (\acc x -> acc || p x) False
-- any p = foldr ((||) . p) False

all :: (a -> Bool) -> [a] -> Bool
all p = foldl (\acc x -> acc && p x) True
-- all p = foldr ((&&) . p) True

and :: [Bool] -> Bool
and = foldl (&&) True

or :: [Bool] -> Bool
or = foldl (||) False

minimum :: Ord a => [a] -> a
minimum = foldl1 min

maximum :: Ord a => [a] -> a
maximum = foldl1 max

length :: Integral i => [a] -> i
length = foldl (\acc _ -> succ acc) 0

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldl (\acc x -> if p x then x:acc else acc) [] . reverse
-- filter p = foldr (\x acc -> if p x then x:acc else acc) []

map :: (a -> b) -> [a] -> [b]
map f = reverse . foldl (\acc x -> f x:acc) []
-- map f = foldr (\x acc -> f x:acc) []

reverse :: [a] -> [a]
reverse = foldl (flip (:)) [] -- foldl snoc
-- reverse = foldr (\x acc -> acc ++ [x]) []

takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile p = foldl (\acc x -> if p x then x:acc else []) [] . reverse
takeWhile p = foldr (\x acc ->  if p x then x:acc else []) []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = foldl (\acc x -> if p x && null acc then acc else acc ++ [x]) []

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: Integral i => [i] -> (i, Maybe i)
semo = foldl (\acc x -> if even x then (fst acc + x, snd acc) else (fst acc, Just (max x (M.fromMaybe x (snd acc))))) (0, Nothing)

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
-- remdups = foldl (\acc x -> (if null acc || (head acc /= x) then x:acc else acc)) [] . reverse
remdups = foldr (\x acc -> (if null acc || (head acc /= x) then x:acc else acc)) []

safeLast :: [a] -> Maybe a
safeLast = foldl (\_ x -> Just x) Nothing
-- safeLast = foldr (\x acc -> Just x) Nothing . reverse

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = foldl (\acc x -> acc * 10 + x) 0
-- dec2int = foldr (\x acc -> acc * 10 + x) 0 . reverse
