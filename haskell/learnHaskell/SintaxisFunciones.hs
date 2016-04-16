import Data.List

lucky :: (Integral a) => a -> String
lucky 7 = "¡El siete de la suerte!"
lucky x = "Lo siento, ¡no es tu día de suerte!"

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

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Tienes infrapeso ¿Eres emo?"
    | bmi <= normal = "Supuestamente eres normal... Espero que seas feo."
    | bmi <= fat    = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise     = "¡Enhorabuena, eres una ballena!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "La lista es" ++ case xs of []  -> "una lista vacía."
                                              [x] -> "una lista unitaria."
                                              xs  -> "una lista larga."
