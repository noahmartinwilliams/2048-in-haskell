module Swipe where

import Types
import Matrix

subTree :: [[Int]] -> Tree
subTree b = Tree b (swipeDir Up b) (swipeDir Down b) (swipeDir Types.Left b) (swipeDir Types.Right b)

-- Rotates counter clockwise
rotateBoard :: [[Int]] -> [[Int]]
rotateBoard [[a, b, c, d], [e, f, g, h], [i, j, k, l], [m, n, o, p]] = [[d, h, l, p], [c, g, k, o], [b, f, j, n], [a, e, i, m]]

fillRandom :: [[Int]] -> [Tree]
fillRandom b = (intern b (getEmpty b)) where
	intern _ [] = []
	intern b (first : rest) = (subTree (changeElement first 2 b)) : (subTree (changeElement first 4 b)) : (intern b rest)

swipeLeft :: [[Int]] -> [[Int]]
swipeLeft [] = []
swipeLeft (first : rest) =  (intern first 0) : (swipeLeft rest) where
	intern :: [Int] -> Int -> [Int]
	intern [] 0 = []
	intern [] x = 0 : (intern [] (x - 1))
	intern (0 : rest) end = intern rest (end + 1)
	intern (first : 0 : rest) end = intern (first : rest) (end + 1)
	intern (first : second : rest ) end = if first == second then (first + second) : (intern rest (end + 1)) else (first : (intern (second : rest) end))
	intern (first : rest) end = first : (intern rest end)

swipeUp :: [[Int]] -> [[Int]]
swipeUp b =  rotateBoard (rotateBoard (rotateBoard (swipeLeft (rotateBoard b))))

swipeRight :: [[Int]] -> [[Int]]
swipeRight b = rotateBoard (rotateBoard (swipeLeft (rotateBoard (rotateBoard b))))

swipeDown :: [[Int]] -> [[Int]]
swipeDown b = (rotateBoard (swipeLeft (rotateBoard (rotateBoard (rotateBoard b)))))

swipeDir :: Dir -> [[Int]] -> [Tree]
swipeDir Up b = if (swipeUp b) /= b
	then
		fillRandom (swipeUp b)
	else
		[]

swipeDir Down b = if (swipeDown b) /= b
	then
		fillRandom (swipeDown b)
	else
		[]

swipeDir Types.Left b = if (swipeLeft b) /= b
	then
		fillRandom (swipeLeft b)
	else
		[]

swipeDir Types.Right b = if (swipeRight b) /= b
	then
		fillRandom (swipeRight b)
	else
		[]
