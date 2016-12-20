module Main where

import Lib
import Data.List
import Data.Maybe
import Data.Monoid

ruleFile :: FilePath
ruleFile = "./data/rules.txt"

showRange (Range b e) = show b <> " -> " <> show e

main :: IO ()
main = do
  ranges <- readRanges ruleFile
  mapM_ putStrLn $ map showRange $ take 30 $ extendSelf $ map fromJust $ filter isJust ranges
