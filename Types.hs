module Types where

-- Tree board up down left right
data Tree = Tree [[Int]] [Tree] [Tree] [Tree] [Tree]
	deriving (Show, Eq)

rotateBoard :: [[Int]] -> [[Int]]
rotateBoard [[a, b, c, d], [e, f, g, h], [i, j, k, l], [m, n, o, p]] = [[d, h, l, p], [c, g, k, o], [b, f, j, n], [a, e, i, m]]

append :: [a] -> [a] -> [a]
append [] list = list
append (first : rest) list = first : (append rest list)

getEmpty :: [[Int]] -> [(Int, Int)]
getEmpty list = (intern list 0) where
	intern :: [[Int]] -> Int -> [(Int, Int)]
	intern [] _ = []
	intern (first : rest) y = append (intern2 first y 0)  (intern rest (y + 1)) where
		intern2 :: [Int] -> Int -> Int -> [(Int, Int)]
		intern2 [] _ _ = []
		intern2 (0 : rest) y x = (x, y) : (intern2 rest y (x + 1))
		intern2 (_ : rest) y x = (intern2 rest y (x + 1))

changeElement :: (Int, Int) -> Int -> [[Int]] -> [[Int]]
changeElement (x, 0) new (list : rest) = (intern x new list) : rest where
	intern :: Int -> Int -> [Int] -> [Int]
	intern 0 new (_ : rest) = new : rest
	intern x new (head : rest) = head : (intern (x - 1) new rest)
changeElement (x, y) new (list : rest) = list : (changeElement (x, (y - 1)) new rest)

swipeRight :: [[Int]] -> [[Int]]
swipeRight [] = []
swipeRight (first : rest) =  (intern first 0) : (swipeRight rest) where
	intern :: [Int] -> Int -> [Int]
	intern [] 0 = []
	intern [] x = 0 : (intern [] (x - 1))
	intern (0 : rest) end = intern rest (end + 1)
	intern (first : second : rest ) end = if first == second then (first + second) : (intern rest (end + 1)) else (first : second : (intern rest end))
	intern (first : rest) end = first : (intern rest end)

swipeUp :: [[Int]] -> [[Int]]
swipeUp b =  rotateBoard (rotateBoard (rotateBoard (swipeRight (rotateBoard b))))
