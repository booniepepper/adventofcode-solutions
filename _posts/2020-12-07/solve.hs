#!/usr/bin/env runhaskell

import Control.Arrow
import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Tree
import System.IO

main :: IO ()
main = readFile "./input" >>= \file ->
       lines
       >>> filter (\l -> length l > 0)
       >>> map (splitOn " contain ")
       >>> solve
       >>> filter (\tree -> elem "shiny gold" tree)
       >>> length
       >>> (\ n -> n - 1) -- Ignore the "self" case
       >>> show
       >>> putStrLn $ file

solve :: [[String]] -> Forest String
solve rawLines = unfoldForest toNode (map fst cleanLines)
    where
        toNode "no other" = ("no other", [])
        toNode x = (x, precedenceMap M.! x)

        precedenceMap :: M.Map String [String]
        precedenceMap = M.fromList cleanLines

        cleanLines :: [(String, [String])]
        cleanLines = map cleanLine rawLines

        cleanLine [outside, insides]
            = (cleanBit outside, cleanBits . uncomma $ insides)
        cleanBit = head . splitOn " bag"
        cleanBits = map (\entry -> if isDigit (head entry) then unwords . tail . words $ entry else entry ) . map cleanBit
        -- cleanBits = concat . map (\entry -> if isDigit (head entry) then take (read . head . words $ entry) (repeat (unwords . tail . words $ entry)) else [entry] ) . map cleanBit
        uncomma = splitOn ", "

