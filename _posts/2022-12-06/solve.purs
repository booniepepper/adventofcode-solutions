#!/usr/bin/env -S spago script -d node-buffer -d node-fs -d strings -d tuples

module Main where

import Prelude
import Data.String
import Data.Tuple
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
    text <- readTextFile UTF8 "input"
    solve 4 text
    solve 14 text
    where
        solve k = firstKuniq k 0 >>> fst >>> (\n -> n + k) >>> show >>> log

firstKuniq :: Int -> Int -> String -> Tuple Int String
firstKuniq k n s =
    let firstK = take k s in
    if allUniq firstK
        then Tuple n firstK
        else firstKuniq k (n + 1) (drop 1 s)

allUniq :: String -> Boolean
allUniq s
    | length s <= 1 = true
    | contains (Pattern (take 1 s)) (drop 1 s) = false
    | otherwise = allUniq (drop 1 s)

