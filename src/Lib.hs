module Lib
    ( readRanges
    , Range(..)
    , extendSelf
    ) where

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

data Range = Range Int Int deriving Show 

instance Eq Range where
  (Range s1 e1) == (Range s2 e2) = (s1 == s2 && e1 == e2)

instance Ord Range where
  (Range s1 _) < (Range s2 _) = s1 < s2
  (Range s1 _) <= (Range s2 _) = s1 <= s2

splitLines :: String -> [String]
splitLines = splitOn "\n"

splitEnds :: String -> [String]
splitEnds = splitOn "-"

readRanges :: FilePath -> IO [Maybe Range]
readRanges p = do
  ruleString <- readFile p
  return $ map parseRange $ splitLines ruleString

buildRange :: Int -> Int -> Range
buildRange s e = Range (minimum [s, e]) (maximum [s, e])

parseRange :: String -> Maybe Range
parseRange s 
  | all isJust [start, end] = Just (buildRange (fromJust start) (fromJust end))
  | otherwise = Nothing where
     parts = splitEnds s :: [String]
     start = readMaybe (minimum parts) :: Maybe Int
     end   = readMaybe (maximum parts) :: Maybe Int 

squash :: Range -> Range -> Range
squash extender orig
  | next_by || cutting || inside  = buildRange (minimum [s1, s2]) (maximum [e1, e2])
  | otherwise                     = orig where
     (Range s1 e1) = extender
     (Range s2 e2) = orig
     next_by       = (e2 + 1 == s1) || (e1 + 1 == s2)
     cutting       = (s1 <= e2 && s2 <= e1) || (s2 <= e1 && s1 <= e2)
     inside        = (s1 <= s2 && e2 <= e1) || (s2 <= s1 && e1 <= e2)

squashWith :: Range -> [Range] -> [Range]
squashWith = map . squash

extendSelf :: [Range] -> [Range]
extendSelf xs
  | xs == extended  = extended 
  | otherwise       = extendSelf extended where
     extended = nub $ sort $ foldr squashWith xs xs
