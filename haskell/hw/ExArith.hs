module ExArith where

data ArEx = Atom Integer
          | Plus ArEx ArEx
          | Times ArEx ArEx
          | Neg ArEx
  deriving (Eq, Show)

-- example expressions
ex1 = (Atom 23) `Plus` (Atom 2)
ex2 = (Atom 7) `Times` ((Atom 7) `Plus` ((Atom 2) `Times` (Atom 8)))
ex3 = Times ex1 ex2
ex4 = Neg $ ex3 `Plus` ex1
ex5 = (Neg ex1) `Times` (Neg ex4)

-- pretty printer
pretty :: ArEx -> String
pretty (Plus exp1 exp2) = "(" ++ pretty exp1 ++ " + " ++ pretty exp2 ++ ")"
pretty (Times exp1 exp2) = "(" ++ pretty exp1 ++ " * " ++ pretty exp2 ++ ")"
pretty (Neg exp) = " (-" ++ pretty exp ++ ")"
pretty (Atom n) =  show n

-- eval evaluates an expression and returns its value
eval :: ArEx -> Integer
eval (Plus exp1 exp2) = eval exp1 + eval exp2
eval (Times exp1 exp2) = eval exp1 * eval exp2
eval (Neg exp) = - eval exp
eval (Atom n) = n

-- step should make only 1 step of calculation on a given ArEx
step :: ArEx -> ArEx
step = undefined

