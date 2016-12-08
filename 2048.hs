module Main where

import System.Random
import Control.Monad
import Swipe
import Types
import Matrix
import Interface
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)

data Options = Options {
	optVersion :: Bool,
	optColor :: String
	}
	deriving (Show, Eq)

displayVersion :: IO Int
displayVersion = do
	putStrLn "2048 v1.1.0 by Noah Williams"
	return 1

defaultOptions = Options {
	optVersion = True,
	optColor = ""
	}
options :: [OptDescr(Options -> Options)]
options = [
	Option ['v'] ["version"] (NoArg (\opts -> opts {optVersion = True}) ) "display version and exit",
	Option ['c'] ["color"] (ReqArg (\g opts -> opts {optColor = (read g)}) "COLOR") "2 : txtylw"
	]

parseArgs :: IO Options
parseArgs = do
	argv <- getArgs
	case getOpt RequireOrder options argv of
		(opts, [], []) -> return (foldl (flip id) defaultOptions opts)

main :: IO Int
main = do
	options <- parseArgs
	let (Options {optVersion = showVersion, optColor = colors}) = options
	if showVersion 
	then
		displayVersion
	else do
		g <- getStdGen
		goWrapper (fillRandom [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]) (randoms g :: [Int])
