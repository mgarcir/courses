import Data.List

lucky :: (Integral a) => a -> String
lucky 7 = "¡El siete de la suerte!"
lucky x = "Lo siento, ¡no es tu día de suerte!"

--factorial :: (Integral a) => a -> a
--factorial 0 = 1
--factorial n = n * (factorial (n - 1))

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial(x-1)

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "NotImplemented"
head' (xs:_) = xs

tell :: (Show a) => [a] -> String
tell []       = "La lista está vacía"
tell (x:[])   = "La lista tiene un elemento: " ++ show x
tell (x:y:[]) = "La lista tiene dos elementos: " ++ show x ++ " y " ++ show y
tell (x:y:_)  = "La lista es larga. Los primeros dos elementos son: " ++ show x ++ " y " ++ show y


deleteDuplicateZero :: (Integral a) => [a] -> [a]
deleteDuplicateZero [] = []
deleteDuplicateZero (x:[]) = x:[]
deleteDuplicateZero (x:y:xs)
    | x == y    = deleteDuplicateZero (y:xs)
    | otherwise = x : deleteDuplicateZero (y:xs)

deleteDuplicate [] = []
deleteDuplicate (x:[]) = x:[]
deleteDuplicate (x:xs)
    | elem x xs = deleteDuplicate xs
    | otherwise = x : deleteDuplicate xs

removeDuplicateAndOrder xs = sort $ deleteDuplicate xs

scanSum ::  [Int] -> [Int]
scanSum [] = []
scanSum (a:[]) = a:[]
scanSum (a:b:as) = a : scanSum ((a+b):as)
