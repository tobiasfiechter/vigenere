module Shared
(prepareKey, prepareText, replaceNth, genTups, german, sortTup, english) where

import Data.List
import Data.Char

-- prepares the key by repeating it so that it is as long as the plaintext
prepareKey :: [Char] -> [Char] -> [Char]
prepareKey ks ps =  rep (div (length ps) (length ks)) (mod (length ps) (length ks)) ks
				where
					rep 0 rem ks' = take rem ks'
					rep n rem ks' = ks' ++ rep (n-1) rem ks'

-- prepares the plaintext by removing all special characters and numbers making the whole text uppercase
prepareText :: [Char] -> [Char]
prepareText ps = [toUpper p | p <- ps ,isAscii p && isAlpha p]

-- replaces the nth element of a given List with the given element
replaceNth :: Int -> a -> [a] -> [a]
replaceNth index newVal (x:xs)				
	| index <= 0 = newVal:xs
	| otherwise = x:replaceNth (index-1) newVal xs
replaceNth index newVal [] = [newVal]

-- generates Tuples of a given List by counting list elements and returning their occurences
genTups :: Ord a => [a] -> [(a,Int)]
genTups ps = sortBy sortTup (map (\a -> (head a, length a)) $ group $ sort ps)

sortTup (_,x) (_,y) | x > y = LT
					| otherwise = GT

german :: [[Char]]
german = ["ENISRA","PVJYXQ"]

english :: [[Char]]
english = ["ETAOIN","VKJXQZ"]