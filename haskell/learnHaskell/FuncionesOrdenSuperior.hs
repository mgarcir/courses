compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

add :: (Num a) => a -> a -> a
add x y = x + y

inc :: (Num a) => a -> a
inc = add 1

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zipMulti :: (a -> b -> c) -> [a] -> [b] -> [c]
zipMulti  _ [] _ = []
zipMulti  _ _ [] = []
zipMulti f (x:xs) (ys) = map (f x) ys ++ (zipMulti f xs ys)

appFunct :: (t2 -> t -> t1) -> t2 -> [t] -> [t1]
appFunct _ _ [] = []
appFunct f x (y:ys) = f x y : appFunct f x ys

flip' f = g
    where g x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<= x) xs)
        greaterSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ greaterSorted

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort' (filter (<=x) xs)
        biggerSorted = quicksort' (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

listToTest = [5,3,5,6,3,2,3,6,7,9,8,7,6,4,5,6,7,8,9]

largestDivisible :: [Integer]
largestDivisible = (filter p [1..1000000000])
    where p x = isZero (x `mod` 3829)


isZero :: Integer -> Bool
isZero = (== 0)

largestDivisible' :: (Integral a) => a
largestDivisible' = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

--sum (takeWhile (sum <10000) (filter odd (map (^2) [1..])))

listSquare limit = (takeWhile (<limit)  (map (^2) [1..]))

sumSquareFunct seed limit (x:xs)
    | seed < limit && (seed + x) > limit = seed
    | otherwise = sumSquareFunct (seed + x) limit xs

sumSquare limit = sumSquareFunct 0 limit (listSquare limit)
