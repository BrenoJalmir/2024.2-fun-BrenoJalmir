module ExArithV where

-- modify ExArith to allow for variables

-- decide how to represent Assignments:
type Assignment = (String, Int)

data ArExV where
  Atom :: Int -> ArExV
  Plus :: ArExV -> ArExV -> ArExV
  Times :: ArExV -> ArExV -> ArExV
  Neg :: ArExV -> ArExV
  Var :: Assignment -> ArExV
  deriving (Show)

ex1 = (Var ("x", 0)) `Plus` (Atom 23) `Plus` (Atom 2)
ex2 = (Atom 7) `Times` ((Atom 7) `Plus` ((Atom 2) `Times` (Atom 8)))
ex3 = Times ex1 ex2
ex4 = Neg $ ex3 `Plus` ex1
ex5 = (Neg ex1) `Times` (Neg ex4)

-- pretty printer
pretty :: ArExV -> String
pretty (Atom n) = show n
pretty (Plus e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
pretty (Times e1 e2) = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"
pretty (Neg e) = "( -" ++ pretty e ++ ")"
pretty (Var (s, _)) = s

-- eval evaluates an expression and returns its value
-- eval :: ?
eval :: ArExV
eval = undefined 

