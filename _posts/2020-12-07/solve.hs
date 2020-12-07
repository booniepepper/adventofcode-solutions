#!/usr/bin/env runhaskell

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Tree
import System.IO

main :: IO ()
main = do 
        file <- readFile "./input"
        let fileLines = map (splitOn " contain ") . filter (\l -> length l > 0) . lines $ file
        putStrLn . solve1 . forestifySparse $ fileLines
        putStrLn . solve2 . forestifyFull $ fileLines

solve1 :: Forest String -> String
solve1 = filter (\tree -> elem "shiny gold" tree)
       >>> length
       >>> pred -- Ignore the "self" case
       >>> show
       >>> (++ " bags can contain at least 1 \"shiny gold\"")

solve2 :: Forest String -> String
solve2 = filter (\(Node b _) -> b ==  "shiny gold")
       >>> head
       >>> length
       >>> pred -- Ignore the "self" case
       >>> show
       >>> (++ " bags required in \"shiny gold\"")

forestifySparse = forestify False
forestifyFull = forestify True

-- Woof. This should be refactored and/or broken up a bit for real code.
forestify :: Bool -> [[String]] -> Forest String
forestify isFull rawLines = unfoldForest toNode (map fst cleanLines)
    where
        toNode x = (x, filter (/= "no other") $ precedenceMap M.! x)

        precedenceMap :: M.Map String [String]
        precedenceMap = M.fromList cleanLines

        cleanLines :: [(String, [String])]
        cleanLines = map cleanLine rawLines

        cleanLine [outside, insides]
            = (cleanBit outside, (if isFull then cleanBitsFull else cleanBitsSparse) . uncomma $ insides)
        cleanBit = head . splitOn " bag"

        cleanBitsSparse = map (\entry -> if isDigit (head entry) then unwords . tail . words $ entry else entry ) . map cleanBit
        cleanBitsFull = concat . map (\entry -> if isDigit (head entry) then take (read . head . words $ entry) (repeat (unwords . tail . words $ entry)) else [entry] ) . map cleanBit
        uncomma = splitOn ", "

