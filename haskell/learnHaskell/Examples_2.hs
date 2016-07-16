add:: Int -> Int -> Int
add x y = x + y

inc::Int -> Int
inc = (+1)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice:: (a -> a) -> a -> a
applyTwice f x = f (f x)

flip':: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = g
    where g x y = f y x

generateOddsList:: Int -> [Int]
generateOddsList x = filter (\y -> odd y) [1..x] 
                           
quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lessThan ++ [x] ++ quicksort greaterThan
  where lessThan = filter (\elem -> elem <= x) xs
        greaterThan = filter (\elem -> elem > x) xs

largestDivisible:: (Integral a) => a
largestDivisible = head $ filter p [100000,99999..1]
   where p x = x `mod` 3829 == 0

squaresSum:: (Integral a) => a
squaresSum = 
 let squaresList = map (^2)
     filterOdds = filter odd
     take10000 = takeWhile (<10000) 
 in (sum . take10000 . filterOdds . squaresList) [1..] 

squaresSum':: (Integral a) => a
squaresSum' = sum $ takeWhile (<10000) $ [x^2 | x <- [1..], odd x]

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain (x:xs) = 
