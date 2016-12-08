module Types where

-- Tree board up down left right
data Tree = Tree [[Int]] [Tree] [Tree] [Tree] [Tree]
	deriving (Show, Eq)


data Dir = Up | Down | Left | Right
	deriving (Show, Eq)
