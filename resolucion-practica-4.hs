-- Ejercicio 1

fibonacci:: Integer -> Integer
fibonacci x | x == 0 = 0
            | x == 1 = 1
            | otherwise = fibonacci(x - 1) + fibonacci(x - 2)


-- Ejercicio 2
parteEntera:: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = parteEntera(n - 1) + 1

-- Ejercicio 3 
esDivisible:: Integer -> Integer -> Bool 
esDivisible x y | x < y = False
                | x == y = True 
                | otherwise = esDivisible (x - y ) y

-- Ejercicio 4
