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
    first4uniq 0 >>> fst >>> (\n -> n + 4) >>> show >>> log $ text


first4uniq :: Int -> String -> Tuple Int String
first4uniq n s =
    let first4 = take 4 s in
    if allUniq first4
        then Tuple n first4
        else first4uniq (n + 1) (drop 1 s)

allUniq :: String -> Boolean
allUniq s
    | length s <= 1 = true
    | contains (Pattern (take 1 s)) (drop 1 s) = false
    | otherwise = allUniq (drop 1 s)

