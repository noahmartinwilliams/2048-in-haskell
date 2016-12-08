module Interface where

import Types
import Swipe

gameOver :: [[Int]] -> Bool
gameOver b = if (swipeUp b) == (swipeDown b) && (swipeDown b) == (swipeLeft b) && (swipeLeft b) == (swipeRight b) then True else False


invalidMove :: Tree -> [Int] -> IO Int
invalidMove t r = do
	putStrLn "Invalid move. Try again."
	return 1

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

goIntern :: [Tree] -> [Int] -> IO Int
goIntern t (random : rest) = go (t !! (mod (abs random) (length t))) rest

go :: Tree -> [Int] -> IO Int
go (Tree board up down left right) random= do
	printBoard board
	if gameOver board
	then
		return 3
	else do
		putStrLn "move? [wasd, r = redo, l = leave]"
		move <- getLine
		e <- (if move == "w"
		then
			if up == []
			then
				go (Tree board up down left right) random
			else
				goIntern up random
		else if move == "s" 
		then
			if down == []
			then
				go (Tree board up down left right) random
			else
				goIntern down random
		else if move == "a"
		then
			if left == []
			then
				go (Tree board up down left right) random
			else
				goIntern left random
		else if move == "d"
		then
			if right == []
			then
				go (Tree board up down left right) random
			else
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

