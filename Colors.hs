module Colors where

import Control.Monad

data BaseColor = Ylw | Blu | Pur | Cyn | Wht | Blk | Red | Grn
	deriving (Show, Eq)

data Color = Txt BaseColor | Bld BaseColor | Und BaseColor | Bak BaseColor
	deriving (Show, Eq)


printInColor :: Color -> String -> IO()
printInColor c str = putStr ((colorCode c) ++ str ++ "\x1b[0m")

printColorCode :: Color -> IO()
printColorCode c = putStr (colorCode c)

colorCode :: Color -> String

colorCode (Txt Blk) = "\x1b[0;30m"
colorCode (Txt Red) = "\x1b[0;31m"
colorCode (Txt Grn) = "\x1b[0;32m"
colorCode (Txt Ylw) = "\x1b[0;33m"
colorCode (Txt Blu) = "\x1b[0;34m"
colorCode (Txt Pur) = "\x1b[0;35m"
colorCode (Txt Cyn) = "\x1b[0;36m"
colorCode (Txt Wht) = "\x1b[0;37m"

colorCode (Bld Blk) = "\x1b[1;30m"
colorCode (Bld Red) = "\x1b[1;31m"
colorCode (Bld Grn) = "\x1b[1;32m"
colorCode (Bld Ylw) = "\x1b[1;33m"
colorCode (Bld Blu) = "\x1b[1;34m"
colorCode (Bld Pur) = "\x1b[1;35m"
colorCode (Bld Cyn) = "\x1b[1;36m"
colorCode (Bld Wht) = "\x1b[1;37m"

colorCode (Und Blk) = "\x1b[4;30m"
colorCode (Und Red) = "\x1b[4;31m"
colorCode (Und Grn) = "\x1b[4;32m"
colorCode (Und Ylw) = "\x1b[4;33m"
colorCode (Und Blu) = "\x1b[4;34m"
colorCode (Und Pur) = "\x1b[4;35m"
colorCode (Und Cyn) = "\x1b[4;36m"
colorCode (Und Wht) = "\x1b[4;37m"

colorCode (Bak Blk) = "\x1b[40m"
colorCode (Bak Red) = "\x1b[41m"
colorCode (Bak Grn) = "\x1b[42m"
colorCode (Bak Ylw) = "\x1b[43m"
colorCode (Bak Blu) = "\x1b[44m"
colorCode (Bak Pur) = "\x1b[45m"
colorCode (Bak Cyn) = "\x1b[46m"
colorCode (Bak Wht) = "\x1b[47m"

