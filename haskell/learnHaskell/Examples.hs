
oddList x = [y | y <- [1..x], odd y]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

factorial :: Int -> Int
factorial x = product [1..x]

myProduct:: Int -> Int
myProduct x = foldl (*) 1 [1..x]

factorial':: Integer -> Integer
factorial' x = product [1..x]

recFactorial:: Integer -> Integer
recFactorial 0 = 1
recFactorial x = x * recFactorial (x-1)

addVectors:: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (xx, xy) (yx,yy) = (xx + yx, xy + yy)

head':: [a] -> a
head' [] = error "Empty List don´t have head!!!"
head' (x:_) = x

head'':: String -> String
head'' "" = "Cadena vacia"
head'' all@(x:_) = "La primera letra de " ++ all ++ "es " ++ [x]

head''':: [a] -> a
head''' xs = case xs of [] -> error "Empty list"
                        (xs:_) -> xs
capital :: String -> String
capital "" = "¡Una cadena vacía!"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]

length':: (Integral b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' (xs)

sum':: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'':: (Num a) => [a] -> a
sum'' xs = foldr (+) 0 xs


bmiTell :: (RealFloat a) => a -> a-> String
bmiTell height weight
    | bmi <= 18.5 = "Tienes infrapeso ¿Eres emo?"
    | bmi <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
    | bmi <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise   = "¡Enhorabuena, eres una ballena!"
    where bmi = weight / height ^ 2


myComparer:: (Ord a) => a -> a -> Ordering
myComparer a b
    | a > b = GT
    | a < b = LT
    | otherwise = EQ

calcBmis:: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / (height * height)

squares:: [Int]
squares = let square x = x*x
          in [square 2, square 3, square 4]

calcBmis'::(RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi w h | (w,h) <- xs, let bmi weight height = weight / height * height]

quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (y:xs) = quicksort minorsList ++ [y] ++ quicksort mayorsList
  where minorsList = [ x | x <- xs, x <= y]
        mayorsList = [ x | x <- xs, x > y]
