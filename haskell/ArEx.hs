module ArEx where

data ArEx where
  (:+) :: Int -> Int -> ArEx
  (:*) :: Int -> Int -> ArEx
  (:-) :: Int -> ArEx
  (:->) :: ArEx -> [ArEx] -> [ArEx]