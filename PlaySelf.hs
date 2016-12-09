module PlaySelf where

import Interface
import Types
import System.Random
import GHC.Conc
import Colors
import Swipe

playSelf :: [(String, Color)] -> IO Int
playSelf c = do
	g <- getStdGen
	playSelfMain (fillRandom [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]) (randoms g :: [Int]) c

playSelfMain :: [Tree] -> [Int] -> [(String, Color)] -> IO Int
playSelfMain t (r : rest) c =  do
	threadDelay 1000000
	if t /= [] 
	then
		playSelfMain2 (t !! (mod (abs r) (length t)))  rest c
	else
		playSelf c

playSelfMain2 :: Tree -> [Int] -> [(String, Color)] -> IO Int
playSelfMain2 (Tree board up down left right) (r : rest) c = do
	putStr (printBoard board c )
	putStr "\n\n"
	let opts = (filter (\a -> (a /= [])) [up, down, left, right])
	if opts == []
	then
		playSelf c
	else
		playSelfMain (opts !! (mod (abs r) (length opts))) rest c
