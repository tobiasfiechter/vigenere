module Keygen
(genKey) where

import qualified Data.Text as T
import System.Random
import Control.Exception
import Control.Monad
import System.IO
import System.IO.Unsafe
import qualified Data.Text.IO as TI

-- generates a random key with given length
genKey :: Int -> [Char]
genKey leng = mkKey leng []
		where
			mkKey leng key
				| leng > 0 = mkKey (leng-1) key ++ [(unsafePerformIO (getStdRandom (randomR ('A','Z'))))]
				| otherwise = key