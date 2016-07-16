import Data.Monoid

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
--landLeft n (left, right) = (left + n, right)
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
-- landRight n (left, right) = (left, right + n)
landRight n (left, right) 
  | abs ((left) - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x

instance Monad [] where
return x = [x]
mappend f xs = concat (map f xs)
xs >>= f = concat (map f xs)
fail _ = []
