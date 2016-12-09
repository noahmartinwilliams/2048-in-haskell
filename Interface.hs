module Interface where

import Types
import Swipe
import Colors
import System.Random


gameOver :: [[Int]] -> Bool
gameOver b = if (swipeUp b) == (swipeDown b) && (swipeDown b) == (swipeLeft b) && (swipeLeft b) == (swipeRight b) then True else False


invalidMove :: Tree -> [Int] -> IO Int
invalidMove t r = do
	putStrLn "Invalid move. Try again."
	return 1

printNum :: String -> [(String, Color)] -> String
printNum i [] = i
printNum i ((i2, c) : rest) = if i == i2 then ((colorCode c) ++ i ++ "\x1b[0m" ) else printNum i rest

printRowIntern :: Int -> [(String, Color)] -> String
printRowIntern first c = do
	if first == 0 then
		"    |"
	else if first < 10 then
		((printNum (show first) c) ++ "   |")
	else if first < 100
	then
		((printNum (show first) c) ++ "  |")
	else if first < 1000
	then
		((printNum (show first) c) ++ " |")
	else
		((printNum (show first) c) ++ "|")

printRow :: [Int] -> [(String, Color)] -> String
printRow [] _ = "]\n"
printRow (first : rest) c = (printRowIntern first c) ++ (printRow rest c)

printBoard :: [[Int]] -> [(String, Color)] -> String
printBoard [a, b, c, d] colors = "[" ++(printRow a colors) ++ "----------------------\n" ++"[" ++ (printRow b colors) ++ "----------------------\n" ++ "[" ++ (printRow c colors) ++ "----------------------\n" ++ "[" ++ (printRow d colors) ++"----------------------\n" 

gameOverPrompt :: [(String, Color)] -> IO Int
gameOverPrompt colors = do
	putStrLn "Game over. [r = redo, p = play again]"
	input <- getLine
	g <- getStdGen
	case input of 
		"r" -> return 1
		"p" -> goWrapper (fillRandom [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]) (randoms g :: [Int]) colors
		_ -> gameOverPrompt colors

goIntern :: [Tree] -> [Int] -> [(String, Color)] -> IO Int
goIntern t (random : rest) colors = go (t !! (mod (abs random) (length t))) rest colors

go :: Tree -> [Int] -> [(String, Color)] -> IO Int
go (Tree board up down left right) random colors= do
	putStr (printBoard board colors)
	if gameOver board
	then
		gameOverPrompt colors
	else do
		putStrLn "move? [wasd, r = redo, l = leave]"
		move <- getLine
		e <- (if move == "w"
		then
			if up == []
			then
				go (Tree board up down left right) random colors
			else
				goIntern up random colors
		else if move == "s" 
		then
			if down == []
			then
				go (Tree board up down left right) random colors
			else
				goIntern down random colors
		else if move == "a"
		then
			if left == []
			then
				go (Tree board up down left right) random colors
			else
				goIntern left random colors
		else if move == "d"
		then
			if right == []
			then
				go (Tree board up down left right) random colors
			else
				goIntern right random colors
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
			go (Tree board up down left right) random colors
		else 
			return 0

goWrapper :: [Tree] -> [Int] -> [(String, Color)] -> IO Int
goWrapper t (random : rest) colors = do
	go (t !! (mod (abs random) (length t))) rest colors


processColorOpts :: String -> [(String, Color)]
processColorOpts "" = []
processColorOpts s = state0  s "" where
	state0 :: String -> String -> [(String, Color)]
	state0 [] _ = []
	state0 ('0' : rest) i = state0 rest (i ++ "0")
	state0 ('1' : rest) i = state0 rest (i ++ "1")
	state0 ('2' : rest) i = state0 rest (i ++ "2")
	state0 ('3' : rest) i = state0 rest (i ++ "3")
	state0 ('4' : rest) i = state0 rest (i ++ "4")
	state0 ('5' : rest) i = state0 rest (i ++ "5")
	state0 ('6' : rest) i = state0 rest (i ++ "6")
	state0 ('7' : rest) i = state0 rest (i ++ "7")
	state0 ('8' : rest) i = state0 rest (i ++ "8")
	state0 ('9' : rest) i = state0 rest (i ++ "9")
	state0 (':' : rest) i = state1 rest i  where
		state1 :: String -> String -> [(String, Color)]
		state1 [] _ = []
		state1 ('t' : 'l' : rest) i = ((i, (Txt Blk)) : (state0 rest ""))
		state1 ('l' : 'l' : rest) i = ((i, (Bld Blk)) : (state0 rest ""))
		state1 ('u' : 'l' : rest) i = ((i, (Und Blk)) : (state0 rest ""))
		state1 ('b' : 'l' : rest) i = ((i, (Bak Blk)) : (state0 rest ""))
		state1 ('t' : 'r' : rest) i = ((i, (Txt Red)) : (state0 rest ""))
		state1 ('l' : 'r' : rest) i = ((i, (Bld Red)) : (state0 rest ""))
		state1 ('u' : 'r' : rest) i = ((i, (Und Red)) : (state0 rest ""))
		state1 ('b' : 'r' : rest) i = ((i, (Bak Red)) : (state0 rest ""))
		state1 ('t' : 'g' : rest) i = ((i, (Txt Grn)) : (state0 rest ""))
		state1 ('l' : 'g' : rest) i = ((i, (Bld Grn)) : (state0 rest ""))
		state1 ('u' : 'g' : rest) i = ((i, (Und Grn)) : (state0 rest ""))
		state1 ('b' : 'g' : rest) i = ((i, (Bak Grn)) : (state0 rest ""))
		state1 ('t' : 'y' : rest) i = ((i, (Txt Ylw)) : (state0 rest ""))
		state1 ('l' : 'y' : rest) i = ((i, (Bld Ylw)) : (state0 rest ""))
		state1 ('u' : 'y' : rest) i = ((i, (Und Ylw)) : (state0 rest ""))
		state1 ('b' : 'y' : rest) i = ((i, (Bak Ylw)) : (state0 rest ""))
		state1 ('t' : 'b' : rest) i = ((i, (Txt Blu)) : (state0 rest ""))
		state1 ('l' : 'b' : rest) i = ((i, (Bld Blu)) : (state0 rest ""))
		state1 ('u' : 'b' : rest) i = ((i, (Und Blu)) : (state0 rest ""))
		state1 ('b' : 'b' : rest) i = ((i, (Bak Blu)) : (state0 rest ""))
		state1 ('t' : 'p' : rest) i = ((i, (Txt Pur)) : (state0 rest ""))
		state1 ('l' : 'p' : rest) i = ((i, (Bld Pur)) : (state0 rest ""))
		state1 ('u' : 'p' : rest) i = ((i, (Und Pur)) : (state0 rest ""))
		state1 ('b' : 'p' : rest) i = ((i, (Bak Pur)) : (state0 rest ""))
		state1 ('t' : 'c' : rest) i = ((i, (Txt Cyn)) : (state0 rest ""))
		state1 ('l' : 'c' : rest) i = ((i, (Bld Cyn)) : (state0 rest ""))
		state1 ('u' : 'c' : rest) i = ((i, (Und Cyn)) : (state0 rest ""))
		state1 ('b' : 'c' : rest) i = ((i, (Bak Cyn)) : (state0 rest ""))
		state1 ('t' : 'w' : rest) i = ((i, (Txt Wht)) : (state0 rest ""))
		state1 ('l' : 'w' : rest) i = ((i, (Bld Wht)) : (state0 rest ""))
		state1 ('u' : 'w' : rest) i = ((i, (Und Wht)) : (state0 rest ""))
		state1 ('b' : 'w' : rest) i = ((i, (Bak Wht)) : (state0 rest ""))
