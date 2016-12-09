module Main where

import System.Random
import Control.Monad
import Swipe
import Types
import Matrix
import Interface
import Colors
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Data.Foldable
import PlaySelf

printVersionNumber :: IO Int
printVersionNumber = do
	putStrLn "2048 by Noah Williams version 1.4"
	return 1

data Opt = Version | ColorStr String | Random 
	deriving (Show, Eq)

getColorOpt :: [Opt] -> [(String, Color)]
getColorOpt [] = []
getColorOpt ((ColorStr s) : _) = processColorOpts s
getColorOpt (_ : rest) = getColorOpt rest

hasRandomPlayOpt :: [Opt] -> Bool
hasRandomPlayOpt [] = False
hasRandomPlayOpt (Random : _ ) = True
hasRandomPlayOpt (_ : rest) = hasRandomPlayOpt rest

main :: IO Int
main = do
	args <- getArgs
	let (options, _, _) = getOpt RequireOrder [
		(Option ['v'] ["version"] (NoArg Version) "Print version and exit"),
		(Option ['c'] ["color"] (ReqArg (\a -> ColorStr a) "") "set the colors to use"),
		(Option ['p'] ["playself"] (NoArg Random) "Play the game by itself")] args
	let c = getColorOpt options
	if (length options) /= 0 && (options !! 0 ) == Version
	then
		printVersionNumber
	else if (hasRandomPlayOpt options)
	then
		playSelf c
	else do
		g <- getStdGen
		goWrapper (fillRandom [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]) (randoms g :: [Int]) c
