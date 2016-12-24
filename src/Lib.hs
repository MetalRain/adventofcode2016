module Lib
    ( readRanges
    , Range(..)
    , clusterRanges
    , rangeLen
    , nextAfter
    ) where

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

-- Inclusive range between two integers
data Range = Range Int Int deriving Show 

instance Eq Range where
  (Range s1 e1) == (Range s2 e2) = (s1 == s2 && e1 == e2)

-- Ord only considers start of Range
-- may not be useful outside of this toy example
instance Ord Range where
  (Range s1 _) < (Range s2 _) = s1 < s2
  (Range s1 _) <= (Range s2 _) = s1 <= s2

nextAfter :: Range -> Int
nextAfter (Range s e) = e + 1

rangeLen :: Range -> Int
rangeLen (Range s e) = e - s + 1

readRanges :: FilePath -> IO [Range]
readRanges p = do
  str <- readFile p
  return $ catMaybes $ map parseRange $ lines str

buildRange :: Int -> Int -> Range
buildRange s e = Range (minimum [s, e]) (maximum [s, e])

parseRange :: String -> Maybe Range
parseRange str 
  | all isJust [start, end] = Just (buildRange (fromJust start) (fromJust end))
  | otherwise = Nothing where
     parts = splitOn "-" str
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

-- Merges intersecting and adjecent ranges, sorts result
clusterRanges :: [Range] -> [Range]
clusterRanges xs
  | xs == extended  = sort extended 
  | otherwise       = clusterRanges extended where
     extended = nub $ foldr squashWith xs xs
