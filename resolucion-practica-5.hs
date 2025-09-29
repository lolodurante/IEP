quitar :: Eq t => t -> [t] -> [t]
quitar t [] = []
quitar a (x:xs) | a == x = xs
                | otherwise = x : quitar a xs

maximo:: [Integer] -> Integer 
maximo [x] = x
maximo (x:y:xs) | x >= y = maximo (x: xs)
                | otherwise = maximo (y:xs)

minimo :: [Integer] -> Integer 
minimo [x] = x
minimo (x:y:xs) | x <= y = minimo (x:xs)
                | otherwise = minimo (y:xs)

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x] 
ordenar (x) = ordenar (quitar (maximo x) x) ++ [maximo x]
{-
Pasos de ordenar (el paso recursivo)
    1. busca el maximo de la lista [4,3,2,5,1,3,5] -> 5
    2. quita ese maximo (5) de la lista x 
    3. corre ordenar sin el item maximo anterior y asi recursivamente hasta que llegue al caso base
    4. el maximo de cada recursividad se suma a una lista nueva con el maximo 
-}
mutiplicarFilas:: [[Integer]] -> [Integer]
mutiplicarFilas [] = []
mutiplicarFilas (x: xs) = mutiplicarFilas xs ++ [multiplicarCadaFila x]
{-
Funcion que sea recursiva y vaya por todas las listas y otra funcion por dentro que trabaje sobre esa lista particular
-}
multiplicarCadaFila:: [Integer] -> Integer
multiplicarCadaFila [x] = x
multiplicarCadaFila (x:xs) = x * multiplicarCadaFila (xs)

{-
Cantidad de apariciones de un numero en la matriz
-}

cantidadDeApariciones:: [[Integer]] -> Integer -> Integer 
cantidadDeApariciones [] _ = 0
cantidadDeApariciones (x: xs) y = cantidadDeAparicionPorFila x y + cantidadDeApariciones xs y

cantidadDeAparicionPorFila:: [Integer] -> Integer -> Integer 
cantidadDeAparicionPorFila [] _ = 0
cantidadDeAparicionPorFila (x: xs) y | x == y = 1 + cantidadDeAparicionPorFila xs y
                                     | otherwise = cantidadDeAparicionPorFila xs y