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
pretty (Plus e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
pretty (Times e1 e2) = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"
pretty (Neg exp) = " (-" ++ pretty exp ++ ")"
pretty (Atom n) =  show n

-- eval evaluates an expression and returns its value
eval :: ArEx -> Integer
eval (Plus e1 e2) = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Neg exp) = - eval exp
eval (Atom n) = n

-- step should make only 1 step of calculation on a given ArEx
step :: ArEx -> ArEx
step ex@(Plus (Atom _) (Atom _)) = Atom $ eval ex
step ex@(Times (Atom _) (Atom _)) = Atom $ eval ex
step (Plus (Atom n) e2) = Plus (Atom n) $ step e2
step (Times (Atom n) e2) = Times (Atom n) $ step e2
step (Plus e1 e2) = Plus (step e1) e2
step (Times e1 e2) = Times (step e1) e2
step (Neg e) = Neg $ step e
step (Atom n) = Atom n

