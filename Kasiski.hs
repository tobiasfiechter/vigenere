module Kasiski
(kasiskiPairs,findRepeatedSequences) where

import Data.List
import Data.Maybe
import Shared
import Factoriser
import Control.Parallel

-- gives back all possible Key lengths and their occurence
kasiskiPairs :: [Char] -> [(Int,Int)]
kasiskiPairs cs = allPosKeyLengths (findRepeatedSequences cs [])

findRepeatedSequences :: [Char] -> [Int] -> [Int]
findRepeatedSequences cs accu
	| null cs = accu
	| otherwise = findRepeatedSequences (drop 1 cs) (accu ++ sequenceSpacings cs 3 10 []) 
		where 
			sequenceSpacings :: [Char] -> Int -> Int -> [Int] -> [Int]
			sequenceSpacings cs minPatLen maxPatLen accu
				| minPatLen < maxPatLen = sequenceSpacings (drop minPatLen cs) (minPatLen+1) maxPatLen (accu ++ (calcIntervals cs (take minPatLen cs)))
				| minPatLen >= maxPatLen = accu
			
calcIntervals :: [Char] -> [Char] -> [Int]
calcIntervals [] pat = []
calcIntervals cs pat = par callRec curInd ++ callRec
	where
		fiIndex = length pat
		patIndex = findSubstringIndex cs pat
		secIndex = findSubstringIndex (replaceNth (patIndex + fiIndex-1) '0' cs) pat
		callRec = (calcIntervals (drop (fiIndex + patIndex) cs) pat)
		curInd = (filter (>0) [(secIndex) - patIndex])

findSubstringIndex :: [Char] -> [Char] -> Int
findSubstringIndex cs pattern = case findIndex (pattern `isPrefixOf`) (tails cs) of
		Just index -> index
		Nothing -> -1