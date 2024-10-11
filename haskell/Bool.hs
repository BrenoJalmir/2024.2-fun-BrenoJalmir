module Bool where

import Prelude hiding (Bool(..))

data Bool where
  False :: Bool
  True :: Bool
  deriving(Eq, Show)

band :: Bool -> Bool -> Bool
band True True = True
band _ _ = False

bor :: Bool -> Bool -> Bool
bor False False = False
bor _ _ = True

bnot :: Bool -> Bool
bnot True = False
bnot False = True

ifthenelse :: Bool -> a -> a -> a
ifthenelse True x _ = x
ifthenelse False _ y = y