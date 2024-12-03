module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes (x:xs) = case x of 
  Just x -> x:catMaybes xs
  Nothing -> catMaybes xs

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _ = error "Just Nothing"

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just b) = b

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

justMap :: (a -> Maybe b) -> [a] -> [b]
justMap f [] = []
justMap f (a:as)
  | isNothing $ f a = justMap f as
  | otherwise = fromJust (f a):justMap f as

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b f Nothing = b
maybe b f (Just a) = f a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:as) = Just a


tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith (mf:mfs) = case mf of
  Nothing -> tryToModifyWith mfs
  Just mf -> tryToModifyWith mfs . map mf 

