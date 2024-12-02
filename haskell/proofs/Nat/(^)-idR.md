# (^)-idR : $(\exists u)(\forall x)[ x^u = x ]$

Considere u = S O.

Seja n :: Nat.

-- Indução em n

Separe em casos de n:

  -- Base

  Caso n = O:

  Calc:

  ```haskell
  O ^ (S O) 
  = [(^).2 n:=O m:= O]
  (O ^ O) * O
  = [(^).1]
  (S O) * O
  = [(*).1]
  = O
  ```

  -- Passo Indutivo

  Caso n = S n':

  Calc:

  ```haskell
  (S n') ^ (S O)
  = [(*).2 n:=(S n') m:= O]
  ((S n') ^ O) * (S n')
  = [(^).1]
  (S O) * (S n')
  = [(*).2 n:=(S O) m:= n']
  = (S O) + ((S O) * n') -- Presente da recursão => = S n'
  ```
