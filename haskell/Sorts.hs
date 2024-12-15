sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:xs@(y:ys)) = x <= y && sorted xs

sort :: Ord a => [a] -> [a]
sort = mergeSort

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve xs@(x:y:ys) =
  let (us, vs) = halve ys
  in (x:us, y:vs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] vs = vs
merge us [] = us
merge xs@(u:us) ys@(v:vs)
  | u <= v = u:merge us ys
  | otherwise = v:merge xs vs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let (us, vs) = halve xs
  in let (us', vs') = (mergeSort us, mergeSort vs)
  in merge us' vs'

filterParts :: (a -> Bool) -> [a] -> ([a], [a])
filterParts _ [] = ([], [])
filterParts p (x:xs) =
  if p x
    then (x:us, vs)
    else (us, x:vs)
  where
    (us, vs) = filterParts p xs

quickSort :: Ord a => [a] -> [a]
-- quickSort [x] = [x]
quickSort [] = []
quickSort (x:xs) =
  let (smaller, bigger) = filterParts (x >) xs
  in quickSort smaller ++ [x] ++ quickSort bigger

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) =
  if y < x
    then y:insert x ys
    else x:y:ys

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []