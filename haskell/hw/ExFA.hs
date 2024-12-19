{-# LANGUAGE KindSignatures #-}

module FA where

import Prelude hiding
    ( Functor(..)
    , fmap
    , (<$>) , (<$) , ($>) , (<&>)
    , unzip
    , Applicative(..)
    , pure
    , (<*>) , (<*) , (*>) , (<**>)
    , liftA , liftA2 , liftA3
    )

import qualified Data.Functor as F
import qualified Control.Applicative as A


----------------------------------------------------------------
-- Functor
----------------------------------------------------------------

class Functor (f :: * -> *) where

  fmap :: (a -> b) -> (f a -> f b)

  (<$) :: b -> f a -> f b
  (<$) = fmap . const

  {- LAWS

     fmap id = id
     fmap (f . g) = fmap f . fmap g

   -}

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fmap fst xs, fmap snd xs)

void :: Functor f => f a -> f ()
void = (<$) ()

-- syntactic associativity and precedence
infixl 4 <$>, $>, <$
infixl 1 <&>

----  Instances  -----------------------------------------------

-- List
instance Functor [] where
    fmap _ [] = []
    fmap f (x:xs) = f x:fmap f xs
 
-- Maybe
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- (α ×)
instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)

-- (α +)
instance Functor (Either a) where
    fmap _ (Left e) = Left e
    fmap f (Right x) = Right (f x)

-- (r →)
instance Functor ((->) r) where
    fmap f g = f . g

-- IO
instance Functor IO where
    fmap f ax = 
      do
        x <- ax
        pure $ f x 


----------------------------------------------------------------
-- Applicative
----------------------------------------------------------------

class Functor f => Applicative (f :: * -> *) where

  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  {- LAWS

     pure id <*> v = v
     pure f <*> pure x = pure (f x)
     u <*> pure y = pure (($) y) <*> u
     u <*> (v <*> w) = (u <*> v) <*> w

   -}

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA = fmap

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- sequence actions, discarding the value of the first argument
(*>) :: Applicative f => f a -> f b -> f b
x *> y = (id <$ x) <*> y

-- sequence actions, discarding the value of the second argument
(<*) :: Applicative f => f a -> f b -> f a
(<*) = flip (*>)

-- A variant of (<*>) with the types of the arguments reversed.
-- It differs from flip (<*>) in that the effects are resolved
-- in the order the arguments are presented.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

when :: Applicative f => Bool -> f () -> f ()
when True f = f
when False _ = pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless = when . not

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL = undefined

-- syntactic associativity and precedence
infixl 4 <*>, *>, <*, <**>

----  Instances  -----------------------------------------------

-- Maybe
instance Applicative Maybe where
    pure = Just
    Just f <*> mx = f <$> mx
    Nothing <*> _ = Nothing

-- Lists with ambiguous computational aspect (non-determinism):
-- Create an isomorphic copy of the type List a, called Ambiguous a
newtype Ambiguous a = Ambiguous {getAmbiguous :: [a]}

-- show ambiguous lists like this: ?[1,2,3]
instance Show a => Show (Ambiguous a) where
  show (Ambiguous xs) = "?" <> show xs

instance Functor Ambiguous where
    fmap f (Ambiguous (x:xs)) = Ambiguous (f x:fmap f xs)

instance Applicative Ambiguous where
    pure x = Ambiguous [x]
    (<*>) :: Ambiguous (a -> b) -> Ambiguous a -> Ambiguous b
    -- Ambiguous af <*> Ambiguous ax = Ambiguous [f x | f <- af, x <- ax]
    Ambiguous (f:fs) <*> Ambiguous xs = Ambiguous (fmap f xs ++ getAmbiguous (Ambiguous fs <*> Ambiguous xs))
    _ <*> _ = Ambiguous []

-- Lists with temporal computational aspect (sequencial):
-- Create an isomorphic copy of the type List a, called Temporal a
-- (the isomorphism is given by the constructor/wrapper Temporal : List a -> Temporal a)
newtype Temporal a = Temporal {getTemporal :: [a]}

-- show temporal lists like this: →[1,2,3]
instance Show a => Show (Temporal a) where
  show (Temporal xs) = "→" <> show xs

instance Functor Temporal where
    fmap f (Temporal (x:xs)) = Temporal (f x:fmap f xs)

instance Applicative Temporal where
    pure x = Temporal [x]
    -- Temporal fs <*> Temporal xs = Temporal (zipWith ($) fs xs) 

-- IO
instance Applicative IO where
    pure = undefined
    (<*>) = undefined

-- (m ×)
instance Monoid m => Applicative ((,) m) where
    pure = undefined
    (<*>) = undefined

-- (s +)
instance Semigroup s => Applicative (Either s) where
    pure = undefined
    (<*>) = undefined

-- (r →)
instance Applicative ((->) r) where
    pure = undefined
    (<*>) = undefined

