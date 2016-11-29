module Types where

-- Tree board up down left right
data Tree = Tree [[Int]] [Tree] [Tree] [Tree] [Tree]
	deriving (Show, Eq)

rotateBoard :: [[Int]] -> [[Int]]
rotateBoard [[a, b, c, d], [e, f, g, h], [i, j, k, l], [m, n, o, p]] = [[d, h, l, p], [c, g, k, o], [b, f, j, n], [a, e, i, m]]

append :: [a] -> [a] -> [a]
append [] list = list
append (first : rest) list = first : (append rest list)

swipeRight :: [[Int]] -> [[Int]]
swipeRight [] = []
swipeRight (first : rest) =  (intern first 0) : (swipeRight rest) where
	intern :: [Int] -> Int -> [Int]
	intern [] 0 = []
	intern [] x = 0 : (intern [] (x - 1))
	intern (0 : rest) end = intern rest (end + 1)
	intern (first : second : rest ) end = if first == second then (first + second) : (intern rest (end + 1)) else (first : second : (intern rest end))
