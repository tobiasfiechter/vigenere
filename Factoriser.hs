module Factoriser
(allPosKeyLengths) where

import Data.List
import Shared
import Control.Parallel

-- Calculates all possible key lengths of a given List of Kasiski pair intervals
allPosKeyLengths :: [Int] -> [(Int,Int)]
allPosKeyLengths ks = genTups (concat ([primeFactors k | k <- ks])++ (filter (<11) ks))

primes :: [Int]
primes = primes' (2:[3,5..])
	where primes' (x:xs) = x : primes' (filter (notDivisorOf x) xs)
 
notDivisorOf d n = n `mod` d /= 0

factors :: [Int] -> Int -> [Int]
factors qs@(p:ps) n
	| n <= 1 = []
	| m == 0 = p : factors qs d
	| otherwise = factors ps n
		where (d,m) = n `divMod` p

primeFactors :: Int -> [Int]
primeFactors k = filter (\x -> x<11 && x>2) (factors primes k)