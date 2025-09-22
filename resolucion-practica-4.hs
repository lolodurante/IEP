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
sumaImpares:: Integer -> Integer
sumaImpares x | x == 0 = 0
              | otherwise = (2*x - 1) + sumaImpares(x - 1)

-- Ejercicio 5
medioFact:: Integer -> Integer

-- esta funcion correria de x = (n - 1)/ 2 a 0.
-- 3: (2)/2 x = 1
medioFact x | x == 0 = 1
            | x == 1 = 1
            | otherwise = x * medioFact(x - 2)


-- Ejercicio 6 

compararUltimoYAnteultimo :: Integer -> Bool
compararUltimoYAnteultimo x =
    mod x 10 == mod (div x 10) 10

-- Ejercicio 7
todosSonIguales:: Integer -> Bool 
todosSonIguales x | x < 10 = True
                  | compararUltimoYAnteultimo x == True = todosSonIguales(div x 10)
                  | otherwise = False

iesimoDigito:: Integer -> Integer -> Integer
iesimoDigito x y | y == 0 = mod x 10
                 | otherwise = iesimoDigito (div x 10) (y - 1)

-- Ejercicio 8
sumaDigitos:: Integer -> Integer
sumaDigitos x | x < 10 =  x 
              | otherwise = sumaDigitos (div x 10) + (mod x 10)

-- Ejercicio 9 
cantidadDeDigitos:: Integer -> Integer 
cantidadDeDigitos x | x < 10 = 1 
                    | otherwise = cantidadDeDigitos(div x 10) + 1 

primerDigito:: Integer -> Integer
primerDigito x | x < 10 = x 
               | otherwise = primerDigito(div x 10)

sacarPrimerDigito :: Integer -> Integer
sacarPrimerDigito n | n < 10    = 0
                    | otherwise = mod n (10 ^ (cantidadDeDigitos n - 1))


esCapicua:: Integer -> Bool 
esCapicua x | x < 10 = True 
            | (x < 100) && (mod x 10 == div x 10) = True
            | (x < 100) && (mod x 10 /= div x 10 ) = False
            | (mod x 10) == (primerDigito x) = esCapicua (div (sacarPrimerDigito(x)) 10)
            | otherwise = False

sumaPotencias :: Integer -> Integer -> Integer -> Integer 
sumaPotencias q n m = sumaPotenciasAuxiliar q n + sumaPotenciasAuxiliar q m

sumaPotenciasAuxiliar :: Integer -> Integer -> Integer 
sumaPotenciasAuxiliar q x | x == 0 = 0
                          | otherwise = q^x + sumaPotenciasAuxiliar q (x - 1)


menorDivisor:: Integer -> Integer
menorDivisor n | n == 1 = 1 
               | otherwise = menorDivisorDesde n 2

menorDivisorDesde:: Integer -> Integer -> Integer 
menorDivisorDesde n x | mod n x == 0 = x
                      | otherwise = menorDivisorDesde n (x + 1)

esPrimo:: Integer -> Bool
esPrimo x | x == 1 = False
          | x == menorDivisor x = True
          | otherwise = False

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos x
  | x < 2     = False
  | otherwise = verificarSuma x 2 0

verificarSuma :: Integer -> Integer -> Integer -> Bool
verificarSuma objetivo candidato acumulado
  | acumulado == objetivo = True
  | acumulado > objetivo  = False
  | esPrimo candidato     = verificarSuma objetivo (candidato + 1) (acumulado + candidato)
  | otherwise             = verificarSuma objetivo (candidato + 1) acumulado
  