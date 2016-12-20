module Lib
    ( readRanges
    , Range(..)
    ) where

import Data.List.Split
import Data.Maybe
import Text.Read

data Range = Range Int Int 

splitLines = splitOn "\n"
splitEnds = splitOn "-"

readRanges :: FilePath -> IO [Maybe Range]
readRanges p = do
  ruleString <- readFile p
  return $ map parseRange $ splitLines ruleString

parseRange :: String -> Maybe Range
parseRange s 
  | all isJust [start, end] = Just (Range (fromJust start) (fromJust end))
  | otherwise = Nothing where
      parts = splitEnds s :: [String]
      start = readMaybe (minimum parts) :: Maybe Int
      end   = readMaybe (maximum parts) :: Maybe Int

--extendRanges :: Range -> [Range] -> [Range]
--extendRanges r rs = 

--joinRanges :: Range -> [Range] -> [Range]
--joinRanges r rs = 
--  | [] == rs = [r]



