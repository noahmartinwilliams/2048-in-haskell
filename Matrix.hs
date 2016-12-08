module Matrix where

import Types
changeElement :: (Int, Int) -> Int -> [[Int]] -> [[Int]]
changeElement (x, 0) new (list : rest) = (intern x new list) : rest where
	intern :: Int -> Int -> [Int] -> [Int]
	intern 0 new (_ : rest) = new : rest
	intern x new (head : rest) = head : (intern (x - 1) new rest)
changeElement (x, y) new (list : rest) = list : (changeElement (x, (y - 1)) new rest)

getEmpty :: [[Int]] -> [(Int, Int)]
getEmpty list = (intern list 0) where
	intern :: [[Int]] -> Int -> [(Int, Int)]
	intern [] _ = []
	intern (first : rest) y = append (intern2 first y 0)  (intern rest (y + 1)) where
		intern2 :: [Int] -> Int -> Int -> [(Int, Int)]
		intern2 [] _ _ = []
		intern2 (0 : rest) y x = (x, y) : (intern2 rest y (x + 1))
		intern2 (_ : rest) y x = (intern2 rest y (x + 1))

append :: [a] -> [a] -> [a]
append [] list = list
append (first : rest) list = first : (append rest list)

