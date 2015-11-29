import Test.HUnit -- runTestTT name_tests
import Test.QuickCheck -- quickCheck prop_test

media3 x y z = (x + y + z) / 3

--Suma de monedas
sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas v w x y z = v + 2*w + 5*x + 10*y + 20*z

prop_sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Bool
prop_sumaMonedas v w x y z = v + 2*w + 5*x + 10*y + 20*z == sumaMonedas v w x y z

sumaMonedas_test1 = TestCase (assertEqual "sumaMonedas 0 0 0 0 1," (20) (sumaMonedas 0 0 0 0 1))
sumaMonedas_test2 = TestCase (assertEqual "sumaMonedas 0 0 8 0 3," (100) (sumaMonedas 0 0 8 0 3))
sumaMonedas_test3 = TestCase (assertEqual "sumaMonedas 1 1 1 1 1," (38) (sumaMonedas 1 1 1 1 1))
sumaMonedas_tests = TestList [TestLabel "sumaMonedas_test1" sumaMonedas_test1,
	TestLabel "sumaMonedas_test2" sumaMonedas_test2,
	TestLabel "sumaMonedas_test3" sumaMonedas_test3]

--Volumen for a sphere.
volumen_Sphere :: Float -> Float
volumen_Sphere x = (4/3) * pi * x^3

prop_volumen_Sphere :: Float -> Bool
prop_volumen_Sphere x = volumen_Sphere x == (4/3) * pi * x^3

areaDeCoronaCircular r1 r2 = pi*(r2^2 -r1^2)
prop_areaDeCoronaCircular r1 r2 = areaDeCoronaCircular r1 r2 == areaDeCoronaCircular r1 r2
areaDeCoronaCircular_test1 = TestCase (assertEqual "areaDeCoronaCircular 1 2" (9.42477796076938) (areaDeCoronaCircular 1 2))
areaDeCoronaCircular_test2 = TestCase (assertEqual "areaDeCoronaCircular 2 5" (65.97344572538566) (areaDeCoronaCircular 2 5))
areaDeCoronaCircular_test3 = TestCase (assertEqual "areaDeCoronaCircular 3 5" (50.26548245743669) (areaDeCoronaCircular 3 5))
areaDeCoronaCircular_tests = TestList [TestLabel "areaDeCoronaCircular_test1" areaDeCoronaCircular_test1,
	TestLabel "areaDeCoronaCircular_test2" areaDeCoronaCircular_test2,
	TestLabel "areaDeCoronaCircular_test3" areaDeCoronaCircular_test3]

