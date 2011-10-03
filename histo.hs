module Main where

import System
import Char
import qualified Data.Map as Map
import Data.List
import Text.Printf

main :: IO ()
main = do
	--read input to string
	(source:_) <- getArgs
	fileStr <- readFile source
	
	--convert to a list of lowercase words, ignoring punctuation around words
	let normalizedListOfWords = words $ map toLower (stripPunctuation fileStr)
	let maxWordLen = maximum $ map (\word -> length word) normalizedListOfWords
	
	--sort by frequency then alpha, scale to < 80 characters, and print
	let sortedList = freqMapToSortedList $ wordsToFreqMap normalizedListOfWords
	let scaledList = scaleFrequencies sortedList maxWordLen
	printHistogram scaledList

	
--Create a Map <Word, Frequency>:
-- first convert the list of words to a list of tuples (word, 1)
-- then combine those tuples, summing the value when the key matches
wordsToFreqMap :: [String] -> Map.Map String Integer
wordsToFreqMap = Map.fromListWith (+) . map (\word -> (word,1))
	
--Convert the Map back into a List, sorting the new tuples by
--frequency (descending) then word (ascending alpha)
freqMapToSortedList :: Map.Map String Integer -> [(String, Integer)]
freqMapToSortedList = sortBy compareWordFreqTuple . Map.toList

--Scale the list to fit in an 80 pixel width max
-- make sure the top row fits, scale everything else down accordingly,
-- and cull words that have a scaled frequency of 0
scaleFrequencies :: [(String, Integer)] -> Int -> [(String, Integer)]
scaleFrequencies sortedList maxWordLen =
	if maxFreq > spaceRemaining
	then filter ((/=0).snd) $ map (\(word, freq) -> 
		(word, div freq (div maxFreq spaceRemaining))) sortedList
	else sortedList
	where
	spaceRemaining = fromIntegral $ 75 - (maxWordLen + 1)
	maxFreq = snd $ head sortedList
	
--Print a formatted, two-column histogram representation of the final List
printHistogram scaledList =
	mapM_ (\(word, freq) -> printf formatStr
		word (concat $ replicate (fromIntegral freq) "#")) scaledList
	where
	newMaxWordLen = maximum $ map (\(word, freq) -> length word) scaledList
	formatStr = "%-" ++ (show newMaxWordLen) ++ "s %s\n"


--Accessory functions

--custom comparator for tuple sort
compareWordFreqTuple (word1, freq1) (word2, freq2)
	| freq1 < freq2 = GT
	| freq1 > freq2 = LT
	| freq1 == freq2 = compare word1 word2

--filter to remove between-word (but not in-word) punctuation marks
stripPunctuation :: String -> String
stripPunctuation = filter (`notElem` "!#$%&*+./<=>?@\\^|~:;,\"()[]")