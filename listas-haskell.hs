-- Ejercicio 1
-- problema suma (xs : [Int]) : Int {
--   requiere: { True }
--   asegura:  { resultado = la suma de todos los elementos de xs }
-- }
suma :: [Int] -> Int
suma xs | xs == [] = 0
        | otherwise = head xs + suma (tail xs)

-- Ejercicio 2
-- problema producto (xs : [Int]) : Int {
--   requiere: { True }
--   asegura:  { resultado = el producto de todos los elementos de xs
--               y resultado = 1 si xs está vacía }
-- }
producto :: [Int] -> Int
producto xs | xs == [] = 1
            | otherwise = head xs * producto (tail xs)

-- Ejercicio 3
-- problema contar (xs : [a]) : Int {
--   requiere: { True }
--   asegura:  { resultado = cantidad de elementos de xs }
-- }
contar :: [a] -> Int
contar [] = 0
contar (_:xs) = 1 + contar xs

-- Ejercicio 4 (lo borre porque tenia algo que no usamos en la materia)

-- Ejercicio 5
-- problema concatenar (xs : [a], ys : [a]) : [a] {
--   requiere: { True }
--   asegura:  { resultado = lista con todos los elementos de xs seguidos de todos los de ys }
-- }
concatenar :: [a] -> [a] -> [a]
concatenar []

-- Ejercicio 6
-- problema ultimo (xs : [a]) : a {
--   requiere: { xs no es vacía }
--   asegura:  { resultado = último elemento de xs }
-- }
ultimo :: [a] -> a
ultimo = undefined

-- Ejercicio 7
-- problema init' (xs : [a]) : [a] {
--   requiere: { xs no es vacía }
--   asegura:  { resultado = xs sin su último elemento }
-- }
init' :: [a] -> [a]
init' = undefined

-- Ejercicio 8
-- problema reversa (xs : [a]) : [a] {
--   requiere: { True }
--   asegura:  { resultado = lista xs en orden inverso }
-- }
reversa :: [a] -> [a]
reversa = undefined

-- Ejercicio 9
-- problema maximo (xs : [Int]) : Int {
--   requiere: { xs no es vacía }
--   asegura:  { resultado = mayor elemento de xs }
-- }
maximo :: [Int] -> Int
maximo = undefined

-- Ejercicio 10
-- problema minimo (xs : [Int]) : Int {
--   requiere: { xs no es vacía }
--   asegura:  { resultado = menor elemento de xs }
-- }
minimo :: [Int] -> Int
minimo = undefined
