module Main where

import System.Random
-- Tree board up down left right
data Tree = Tree [[Int]] [Tree] [Tree] [Tree] [Tree]
	deriving (Show, Eq)

-- Rotates counter clockwise
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

subTree :: [[Int]] -> Tree
subTree b = Tree b (fillRandom (swipeUp b)) (fillRandom (swipeDown b)) (fillRandom (swipeLeft b)) (fillRandom (swipeRight b))

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

printRowIntern :: Int -> IO()
printRowIntern first = do
	if first == 0 then
		putStr "    |"
	else if first < 10 then
		putStr ((show first) ++ "   |")
	else if first < 100
	then
		putStr ((show first) ++ "  |")
	else if first < 1000
	then
		putStr ((show first) ++ " |")
	else
		putStr ((show first) ++ "|")

printRow :: [Int] -> IO()
printRow [] = do
	putStrLn "]"
printRow (first : rest) = do
	printRowIntern first
	printRow rest

printBoard :: [[Int]] -> IO()
printBoard [a, b, c, d] = do
	putStr "["
	printRow a
	putStrLn "----------------------"
	putStr "["
	printRow b
	putStrLn "----------------------"
	putStr "["
	printRow c
	putStrLn "----------------------"
	putStr "["
	printRow d 
	putStrLn "----------------------"

invalidMove :: Tree -> [Int] -> IO Int
invalidMove t r = do
	putStrLn "Invalid move. Try again."
	return 1

goIntern :: [Tree] -> [Int] -> IO Int
goIntern t (random : rest) = go (t !! (mod (abs random) (length t))) rest

go :: Tree -> [Int] -> IO Int
go (Tree board up down left right) random= do
	printBoard board
	putStrLn "move? [wasd, r = redo, l = leave]"
	move <- getLine
	e <- (if move == "w"
	then
		goIntern up random
	else if move == "s" 
	then
		goIntern down random
	else if move == "a"
	then
		goIntern left random
	else if move == "d"
	then
		goIntern right random
	else if move == "r"
	then
		return 1
	else if move == "l"
	then
		return 3
	else
		invalidMove (Tree board up down left right) random)

	if e == 1
	then
		return 2
	else if e == 2
	then
		go (Tree board up down left right) random
	else 
		return 0

goWrapper :: [Tree] -> [Int] -> IO Int
goWrapper t (random : rest) = do
	go (t !! (mod (abs random) (length t))) rest

main :: IO Int
main = do
	g <- getStdGen
	goWrapper (fillRandom [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]) (randoms g :: [Int])
