module List where

import Prelude hiding ((.), (++), head, tail, length, foldr, foldl, flip, map, filter, reverse, take, repeat, any, dropWhile, concat, replicate, zip, subseqs, init, inits)

import Nat

data List a where
  Nil :: List a
  (:>) :: a -> List a -> List a
  deriving (Eq, Show)

head :: [a] -> a
head [] = error "head of null"
head (a:as) = a

tail :: [a] -> [a]
tail [] = error "tail of null"
tail (a:as) = as

null :: [a] -> Bool
null [] = True
null _ = False

length :: [a] -> Integer
length [] = 0
length (a:as) = succ $ length as

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) = f a: map f as

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (a:as)
  | p a = a:filter p as
  | otherwise = filter p as

foldr :: (a -> a -> a) -> a -> [a] -> a
foldr _ nEl [] = nEl
foldr f nEl (a:as) = a `f` foldr f nEl as

foldl :: (a -> a -> a) -> a -> [a] -> a
foldl _ nEl [] = nEl
foldl f nEl (a:as) = foldl f nEl as `f` a

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) a =  f (g a)

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

(++) :: [a] -> [a] -> [a]
[] ++ as = as
(a:as) ++ bs = a:(as ++ bs)

reverse :: [a] -> [a]
reverse [] = []
reverse (a:as) = reverse as ++ [a]
-- reverse as = foldr (\a -> (++) [a]) [] as

snoc :: a -> [a] -> [a]
snoc a [] = [a]
snoc b (a:as) = a:snoc b as

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys
-- xs +++ (y:ys) = (xs <: y) +++ ys

take :: Nat -> [a] -> [a]
take (S n) (a: as) = a:take n as
take _ [] = []

repeat :: a -> [a]
repeat a = a:repeat a

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (||) False . map p
-- any p = foldr ((||) . p) False  -- erro por causa implementação do foldr 'simples'

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p as@(a:as')
  | p a = dropWhile p as'
  | otherwise = as

concat :: [[a]] -> [a]
-- concat [] = []
concat = foldr (++) []

replicate :: Nat -> a -> [a]
replicate O _ = []
replicate (S n) a = a:replicate n a

zip :: [a] -> [b] -> [(a, b)]
zip (a:as) (b:bs) = (a, b):zip as bs
zip _ _ = []

-- zip em termos de zipWith
zip' :: [a] -> [b] -> [(a, b)]
zip' = zW (, )
-- zip' = zW (\x y -> (x, y))

subseqs :: [a] -> [[a]]
subseqs = undefined

-- zipWith
zW :: (a -> b -> c) -> [a] -> [b] -> [c]
zW op (a:as) (b:bs) = (a `op` b):zW op as bs
zW _ _ _ = []

-- zipWith em termos de zip
zW' :: (a -> b -> c) -> [a] -> [b] -> [c]
zW' op as bs = map (uncurry op) (zip as bs)
-- zW' op as = map (uncurry op) . zip as 

-- (errado) feito usando desconstrutor
-- pairs :: [a] -> [(a, a)]
-- pairs (a:[]) = []
-- pairs (a:as) = (a, head as):pairs as

pairs :: [a] -> [(a, a)]
pairs [a] = []
pairs (a:as@(a':as')) = (a, a'):pairs as

init :: [a] -> [a]
init as = reverse $ tail $ reverse as

inits :: [a] -> [[a]]
inits [] = [[]]
inits as@(a:as') = inits (init as)<:as

countdown :: Nat -> [Nat]
countdown O = [O]
countdown (S n) = n:countdown n