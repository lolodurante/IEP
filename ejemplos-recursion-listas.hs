sumatoria :: [Integer] -> Integer 
sumatoria xs 
  | xs == []  = 0
  | otherwise = head xs + sumatoria (tail xs)


pertenece :: Int -> [Int] -> Bool 
pertenece y xs
  | xs == []        = False 
  | y == head xs    = True
  | otherwise       = pertenece y (tail xs)

longitudConPatternMatching :: [Int] -> Int
longitudConPatternMatching []     = 0
longitudConPatternMatching (_:xs) = 1 + longitudConPatternMatching xs

sumatoriaconpatternmatching:: [Int] -> Int 
sumatoriaconpatternmatching []      = 0 
sumatoriaconpatternmatching (x: xs) = sumatoriaconpatternmatching xs + x

