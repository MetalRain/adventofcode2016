module Main where

import Lib
import Data.Maybe
import Data.Monoid

ruleFile :: FilePath
ruleFile = "./data/rules.txt"

showRange (Range b e) = show b <> "->" <> show e

main :: IO ()
main = do 
  ranges <- readRanges ruleFile
  print $ map (showRange . fromJust) $ filter isJust ranges
