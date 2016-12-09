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

printVersionNumber :: IO Int
printVersionNumber = do
	putStrLn "2048 by Noah Williams version 1.3"
	return 1

data Opt = Version | ColorStr String
	deriving (Show, Eq)

getColorOpt :: [Opt] -> [(String, Color)]
getColorOpt [] = []
getColorOpt ((ColorStr s) : _) = processColorOpts s
getColorOpt (_ : rest) = getColorOpt rest

main :: IO Int
main = do
	args <- getArgs
	let (options, _, _) = getOpt RequireOrder [
		(Option ['v'] ["version"] (NoArg Version) "Print version and exit"),
		(Option ['c'] ["color"] (ReqArg (\a -> ColorStr a) "") "set the colors to use")] args
	if (length options) /= 0 && (options !! 0 ) == Version
	then
		printVersionNumber
	else do
		let c = getColorOpt options
		g <- getStdGen
		goWrapper (fillRandom [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]) (randoms g :: [Int]) c
