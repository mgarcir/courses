double x = x * x
doubleUs x y = double x + double y
doubleSmallNumber x = if x > 100
						then x
						else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]

generateTriangules = [ (x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10] , y^2 + x^2 == z^2, x + y +z == 24]

