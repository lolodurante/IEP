-- Ejercicio 1 a-- 
f:: Integer -> Integer
f 1 = 8 
f 4 = 131
f 16 = 16

-- Ejercicio 1 b -- 
g:: Integer -> Integer
g 8 = 16
g 16 = 4 
g 131 = 4

-- Ejercicio 1 c --
h:: Integer -> Integer
h x = f (g x )

k::Integer -> Integer
k x = g (f x )

-- Ejercicio 2 a-- 
absoluto:: Integer -> Integer
absoluto x | x >= 0 = x
           | otherwise = - x
 
-- Ejercicio 2 b --
maximoAbsoluto:: Integer -> Integer -> Integer
maximoAbsoluto x y | x >= y = x 
                   | otherwise = y

-- Ejercicio 2 c -- 
maximo3:: Integer -> Integer -> Integer -> Integer
maximo3 x y z | (x == y) && (x == z) = x
              | (x >= y) && (x >= z) = x 
              | (y >= x) && (y >= z) = y 
              | (z >= x) && (z >= y) = z

-- Ejercicio 2 d -- 
-- Sin pattern matching
algunoEsCeroSinPatternMatching:: Integer -> Integer -> Integer
algunoEsCeroSinPatternMatching x y | (x == 0) || (y == 0) = 0
                                   | otherwise = -1 

-- Con pattern matching -- 
algunoEsCeroConPatternMatching:: Integer -> Integer -> Bool
algunoEsCeroConPatternMatching 0 _ = True
algunoEsCeroConPatternMatching _ 0 = True 
algunoEsCeroConPatternMatching _ _ = False

-- Ejercicio 2 e -- 
ambosSonCero:: Integer -> Integer -> Bool 
ambosSonCero 0 0 = True 
ambosSonCero _ _ = False


