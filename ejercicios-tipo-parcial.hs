generarStock:: [String] -> [(String, Integer)]
generarStock [] = []
generarStock (x:xs) = sumarATuplasDeApariciones x (generarStock xs) 

sumarATuplasDeApariciones:: String ->[(String, Integer)] -> [(String, Integer)]
sumarATuplasDeApariciones x [] = [(x, 1)]
sumarATuplasDeApariciones x ((a, b):ys) | x == a = (a, b + 1):ys
                                        | otherwise = (a,b):sumarATuplasDeApariciones x ys

--
stockDeProducto:: [(String, Integer)] -> String -> Integer
stockDeProducto [] x = 0
stockDeProducto ((nombre, cantidad):xs) nombreABuscar | nombreABuscar == nombre = cantidad
                                                      | otherwise = stockDeProducto xs nombreABuscar

--
dineroEnStock:: [(String, Integer)] -> [(String, Float)] -> Float
dineroEnStock [] _ = 0
dineroEnStock ((a,b):xs) y = (encontrarPrecioEnStock a y) * (fromIntegral b) + (dineroEnStock xs y )

encontrarPrecioEnStock:: String -> [(String, Float)] -> Float
encontrarPrecioEnStock _ [] = 0 
encontrarPrecioEnStock nombre ((a,b):xs) | a == nombre = b
                                         | otherwise = encontrarPrecioEnStock nombre xs

--
aplicarOferta::[(String, Integer)] -> [(String, Float)] -> [(String, Float)]
aplicarOferta [] [] = []
aplicarOferta _ [] = []
aplicarOferta [] _ = []
aplicarOferta ((a,b):xs) y | encontrarPrecioEnStock a y >= 10.0 = ( a, (encontrarPrecioEnStock a y )*0.8 ):aplicarOferta xs y
                           | encontrarPrecioEnStock a y == 0 
                           | otherwise = (a, encontrarPrecioEnStock a y ):aplicarOferta xs y



esParEnPosicion :: Integer -> [Integer] -> Bool
esParEnPosicion _ [] = False
esParEnPosicion y (x:xs)
  | (y == 1) && (mod x 2 == 0) = True
  | (y == 1) && (mod x 2 /= 0) = False
  | otherwise = esParEnPosicion (y - 1) xs
