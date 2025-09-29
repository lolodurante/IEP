relacionesValidas:: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:xs)
                     | (checkearQueEsteRepetido x xs == True || tuplaTieneAmbosElementosIguales x == True) = False
                     | otherwise = relacionesValidas (xs)


checkearQueEsteRepetido:: (String, String) -> [(String, String)] -> Bool 
checkearQueEsteRepetido _ [] = False
checkearQueEsteRepetido x (y:ys) | validarSiTuplasSeRepiten x y == True = True 
                                 | otherwise = checkearQueEsteRepetido x ys

tuplaTieneAmbosElementosIguales:: (String, String) -> Bool 
tuplaTieneAmbosElementosIguales x | fst x == snd x = True
                                  | otherwise = False


validarSiTuplasSeRepiten:: (String, String) -> (String, String) -> Bool 
validarSiTuplasSeRepiten x y | (fst x == fst y || fst x == snd y) && (snd x == fst y || snd x == snd y) = True 
                             | otherwise = False

pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece s (x:xs)
  | s == x    = True
  | otherwise = pertenece s xs

insertarSiNoEsta :: String -> [String] -> [String]
insertarSiNoEsta s xs
  | pertenece s xs = xs
  | otherwise      = s : xs

personas :: [(String, String)] -> [String]
personas [] = []
personas ((a, b):xs) = insertarSiNoEsta a (insertarSiNoEsta b (personas xs))

--- amigos de
-- array en los que aparece ela persona 
amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe x ((a,b):ys)
  | x == a = b : amigosDe x ys
  | x == b = a : amigosDe x ys
  | otherwise = amigosDe x ys

--
aplanarTupla:: [(String, String)] -> [String]
aplanarTupla [] = []
aplanarTupla ((a, b):xs) = aplanarTupla xs ++ [a, b]

-- convierte [(nombre,nombre)] -> [nombre]
aplanar :: [(String, String)] -> [String]
aplanar [] = []
aplanar ((a,b):xs) = a : b : aplanar xs

-- tu actualizarContador ajustado levemente (opcional)
actualizarContador :: String -> [(String, Integer)] -> [(String, Integer)]
actualizarContador x [] = [(x, 1)]
actualizarContador x ((a,b):ys)
  | x == a    = (a, b + 1) : ys
  | otherwise = (a, b) : actualizarContador x ys

-- selector con caso vacío
seleccionarPersonaConMasAmigos :: [(String, Integer)] -> String
seleccionarPersonaConMasAmigos [] = ""          -- decide cómo manejar vacío
seleccionarPersonaConMasAmigos [(a,_)] = a
seleccionarPersonaConMasAmigos ((a,b):(c,d):xs)
  | b >= d    = seleccionarPersonaConMasAmigos ((a,b):xs)
  | otherwise = seleccionarPersonaConMasAmigos ((c,d):xs)

-- ahora sí, encastre correcto de tipos
personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos rels =
  seleccionarPersonaConMasAmigos (contarFrecuencias (aplanar rels))



