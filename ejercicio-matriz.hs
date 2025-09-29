esParEnPosicion :: Integer -> [Integer] -> Bool
esParEnPosicion _ [] = False
esParEnPosicion y (x:xs)
  | (y == 1) && (mod x 2 == 0) = True
  | (y == 1) && (mod x 2 /= 0) = False
  | otherwise = esParEnPosicion (y - 1) xs


parEnPosicion:: Integer -> [[Integer]] -> Integer 
parEnPosicion _ [] = 0
parEnPosicion y (x:xs) | esParEnPosicion y x  = 1 + parEnPosicion y xs 
                       | otherwise = parEnPosicion y xs