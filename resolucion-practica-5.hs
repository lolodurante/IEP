-- Ejercicio 1
longitud:: [t] -> Integer
longitud [] = 0
longitud (_:xs) = longitud(xs) + 1
