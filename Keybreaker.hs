module Keybreaker
(breakKey) where

import Data.List.Split.Internals
import Data.Function
import Shared
import Decrypt
import Data.List

breakKey :: [Char] -> Int -> [[Char]]
breakKey cs kl = sequence (rmTups [(caesarWithScore block) |block <- (charFrequency cs kl)])

-- takes the ciphertext and a possible key length and returns the frequency of all characters in a chunck of characters created by taking every kl character of cs
charFrequency :: [Char] -> Int -> [[(Char, Int)]]
charFrequency cs kl = [genTups chunk | chunk <- buildBlocks cs kl]

-- changes all tuples to a list of [Char]
rmTups :: [[(Char, Int)]] -> [[Char]]
rmTups tll = [ [c | (c,_) <- tl] | tl <- tll]

-- Builds blocks as every character at keylength is in one block
buildBlocks :: [Char] -> Int -> [[Char]]
buildBlocks cs kl = buildList cs kl [] 0
	where
		buildList :: [Char] -> Int -> [[Char]] -> Int-> [[Char]]
		buildList cs kl accu i
			| i >= (length cs) = accu
			| (length accu) <= (mod i kl) = buildList cs kl (accu ++ [[(cs !! i)]]) (i+1)
			| otherwise = buildList cs kl (replaceNth (mod i kl) ((accu !! (mod i kl)) ++ [(cs !! i)]) accu) (i+1)

-- takes a list of characters and theis occurence and slide them through the whole alphabet and calculating a score which provides 
-- evidence of which Letter it has been encrypted if the sliding index is the right one
caesarWithScore :: [(Char,Int)] -> [(Char,Int)]
caesarWithScore cs = 
	let sortedTups = (sortBy sortTup [calcScore (buildList cs c) c| c <- ['A' .. 'Z']])
	in takeHighestTups sortedTups

buildList :: [(Char,Int)] -> Char -> [(Char,Int)]
buildList cs c = [(((decrypt [c] [cha])!!0),i)|(cha,i) <- cs]

-- compares tuples and takes the tuples which provide highest occurence
takeHighestTups :: [(Char,Int)] -> [(Char,Int)]
takeHighestTups tups = compareTups (head tups) (tail tups) (snd(head tups)) []
	where
		compareTups :: (Char,Int) -> [(Char,Int)] -> Int ->[(Char,Int)] -> [(Char,Int)]
		compareTups (cfi,ifi) tups highest accu
			| null tups = accu
			| highest == snd(head tups) = compareTups (head tups) (tail tups) ifi (accu ++ [(cfi,ifi)])
			| highest == (snd(head tups)+1) = compareTups (head tups) (tail tups) highest (accu ++ [(cfi,ifi)])
			| otherwise = (accu ++ [(cfi,ifi)])

-- calculates a score if a specified sliding character is the right one for the given language
calcScore :: [(Char,Int)] -> Char -> (Char, Int)
calcScore pdcs c = (c,((containsInLang (take 6 pdcs) (english !! 0)) + (containsInLang (take 6 (reverse pdcs)) (english !! 1))))
	where 
		containsInLang :: [(Char,Int)] -> [Char] -> Int
		containsInLang elems lang = sum ([checkAndAccu c lang | (c,_) <- elems])
			where
				checkAndAccu :: Char -> [Char] -> Int
				checkAndAccu c lang
					| c `elem` lang = 1
					| otherwise = 0