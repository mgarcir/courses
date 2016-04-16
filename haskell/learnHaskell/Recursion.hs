--maximun' [] = []
--maximun' [x] = x
--maximun' [x:xs]
--    | x > maxtail = x
--    | otherwise = maxtail
--    where maxtail = maximun' xs


--maximum'' :: (Ord a) => [a] -> a
--maximum'' []     = error "maximum of empty list"
--maximum'' [x]    = x
--maximum'' [x:xs] = x `max` (maximum'' xs)

fibbonaci 0 = 0
fibbonaci 1 = 1
fibbonaci x =  fibbonaci(x-1) + fibbonaci (x-2)

fibList x = [fibbonaci(y) | y <- [1..x]]

replicate' x y
    | y <= 0 = []
    | otherwise = x : replicate' x (y-1)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        greaterSorted = quicksort [a | a <- xs, a >  x]
    in  smallerSorted ++ [x] ++ greaterSorted
