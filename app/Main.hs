module Main where

import Lib

ruleFile :: FilePath
ruleFile = "./data/rules.txt"

showResults :: [Range] -> IO ()
showResults ranges = do mapM_ putStrLn results where
  clustered   = clusterRanges ranges
  firstFree   = nextAfter $ clustered !! 0
  nBlocked    = foldr (+) 0 $ map rangeLen clustered 
  allowedIPs  = rangeLen(Range 0 4294967295) - nBlocked
  results     = [ "Part one: "
                , show firstFree
                , "Part two: "
                , show allowedIPs
                ]

main :: IO ()
main = do
  ranges <- readRanges ruleFile
  showResults ranges
 
