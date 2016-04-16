{-
If we list all the nat­ural num­bers below 10 that are mul­ti­ples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these mul­ti­ples is 23.
Find the sum of all the mul­ti­ples of 3 or 5 below 1000.
-}
import Data.List (union)
multiple' = sum $ union [3,6..999] [5,10..999]

multiple = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

multiple'' = sumStep 3 999 + sumStep 5 999 - sumStep 15 999
  where
    sumStep s n = s * sumOnetoN (n `div` s)
    sumOnetoN n = n * (n+1) `div` 2
