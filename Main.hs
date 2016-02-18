import Decrypt
import Encrypt
import Kasiski
import Keybreaker
import Shared
import Keygen
import Control.Parallel
import Data.Char

import System.Random
main = do
	putStrLn "Welcome to the cryptanalysis of the vigenere cipher!"
	getLine
	putStrLn "First we need to read some plaintext. Here it is:"
	plainTextFile <- readFile "plaintext.txt"
	putStrLn plainTextFile
	getLine
	putStrLn "Before we can encrypt this text, we will need to prepare it. Here is the prepared plaintext:"
	let preparedPlainText = prepareText plainTextFile
	putStrLn (preparedPlainText)
	getLine
	putStrLn "Now we are ready to encrypt the plaintext you can choose a KEY or generate one. write a KEY or gen to generate one"
	key <- getLine
	if(key == "gen") 
		then
			do
				putStrLn "Choose a KEY length between 2 - 10"
				keyLength <- readLn
				let keyGenerated = genKey keyLength
				putStrLn ("Your KEY is: " ++ keyGenerated)
				writeFile "ciphertext.txt" (encrypt keyGenerated preparedPlainText)
		else
			do
				putStrLn ("Your KEY is: " ++ (prepareText key))
				writeFile "ciphertext.txt" (encrypt key preparedPlainText)
	cipherTextFile <- readFile "ciphertext.txt"
	let kasPairs = kasiskiPairs cipherTextFile
	putStrLn "Your ciphertext is:"
	putStrLn (par kasPairs cipherTextFile)
	getLine
	putStrLn "To break the ciphertext we have to find kasiski pairs. Right now we are calculating some possible KEY lengths using kasiski pairs."
	getLine
	putStrLn "Here they are: "
	let tryWithKeyLength = 
		do
			print kasPairs
			putStrLn "Now we can try to break the KEY for a given KEY length. The KEY lengths can be taken from the kasiski pairs above. Choose a KEY length"
			keyLength <- readLn
			let posKeys = breakKey cipherTextFile keyLength
			putStrLn ("These are possible keys with length " ++ [(intToDigit keyLength)])
			print posKeys
			putStrLn "If you want to try another KEY length write retry"
			action <- getLine
			if(action == "retry")
				then
					do
						tryWithKeyLength
				else
					do
						putStrLn "Enter chosen KEY:"
						hackedKey <- getLine
						putStrLn "Here is the decrypted Text"
						putStrLn (decrypt hackedKey cipherTextFile)
	tryWithKeyLength
	putStrLn "FINISH"
	getLine