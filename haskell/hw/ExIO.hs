module ExIO where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )

-- read through the whole module first, to get an idea
-- of what's required and to decide where to start

getLine :: IO String
getLine =
  do
    c <- getChar
    if c == '\n'
      then pure []
      else do
        str <- getLine
        pure (c:str)

getInt :: IO Int
getInt =
  do
    str <- getLine
    pure (read str :: Int)

getSafeInt :: IO (Maybe Int)
getSafeInt =
  do
    str <- getLine
    pure (case reads str :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing)

-- sequencing: first do f ignoring its result, then do g and keep its result
infixl 1 >>

(>>) :: IO a -> IO b -> IO b
ax >> ay =
  do
    ax
    y  <- ay
    pure y

-- pauses till the user presses any normal key
pause :: IO ()
pause =
  do
    getChar
    pure ()

skip :: IO ()
skip = do pure ()

newline :: IO ()
newline = putChar '\n'

-- define it as a foldr
putStr :: String -> IO ()
putStr = foldr (\c acc -> putChar c >> acc) (pure ())

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize f x =
  do
    b <- f x
    newline
    pure b

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

-- reads the entire user input as a single string, transforms it, and prints it
interact :: (String -> String) -> IO ()
interact f =
  do
    str <- getLine
    putStrLn (f str)

perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = interact . perlineize

when :: Bool -> IO () -> IO ()
when True f = do f
when False _ = skip

unless :: Bool -> IO () -> IO ()
unless = when . not

guard :: Bool -> IO ()
guard False = skip
guard True = pause

forever :: IO a -> IO b
forever ax = do forever ax

-- transforms the action given to an equivalent one that has no result
void :: IO a -> IO ()
void ax =
  do
    ax
    putStr ""


-- Kleisli compositions
infixr 1 >=>, <=<

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
(f >=> g) x =
  do
    y <- f x
    g y

-- traditional order
-- comparison of types:
-- (.)   :: (b ->    c) -> (a ->    b) -> a ->    c
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> a -> IO c
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)


-- Bind
infixl 1 >>=

(>>=) :: IO a -> (a -> IO b) -> IO b
ax >>= f =
  do
    x <- ax
    f x

infixl 4 $>, <$

-- make an action that has the side effects of the action on the left
-- but with result the value on the right
($>) :: IO a -> b -> IO b
ax $> y =
  do
    ax
    pure y

-- vice-versa
(<$) :: a -> IO b -> IO a
x <$ ioy = ioy $> x

ap :: IO (a -> b) -> IO a -> IO b
af `ap` ax =
  do
    f <- af
    f <$> ax
-- af `ap` ax = af <*> ax

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO _ [] = pure []
filterIO p (x:xs) =
  do
    b <- p x
    xs' <- filterIO p xs
    if b
      then pure (x:xs')
      else pure xs'

iomap :: (a -> b) -> IO a -> IO b
iomap f ax =
  do
    f <$> ax
    -- x <- ax
    -- pure (f x)
-- iomap = (<$>)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f (x:xs) =
  do
    x' <- f x
    xs' <- mapIO f xs
    pure (x': xs')
mapIO _ _ = pure []

zipWithIO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithIO f (x:xs) (y:ys) =
  do
    ws <- zipWithIO f xs ys
    w <- f x y
    pure (w:ws)
zipWithIO _ _ _ = pure []

zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ f xs ys = void $ zipWithIO f xs ys

sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = pure []
sequenceIO (ax:axs) =
  do
    x <- ax
    xs <- sequenceIO axs
    pure (x:xs)

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = void . sequenceIO

replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO n ax =
  if n <= 0
    then pure []
    else
      do
        x <- ax
        xs <- replicateIO (n-1) ax
        pure (x:xs)

replicateIO_ :: Integral i => i -> IO a -> IO ()
replicateIO_ n ax = void $ replicateIO n ax

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO [] _ = pure []
forIO (x:xs) f =
  do
    y <- f x
    ys <- forIO xs f
    pure (y:ys)

forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ [] _ = pure ()
forIO_ (x:xs) f =
  do
    void $ f x
    forIO_ xs f

joinIO :: IO (IO a) -> IO a
joinIO aax = 
  do 
    x <- aax 
    x

foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO _ acc [] = pure acc
foldlIO f acc (x:xs) = 
  do
    acc' <- f acc x
    foldlIO f acc' xs

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ f acc = void . foldlIO f acc
