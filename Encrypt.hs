module Encrypt
(encrypt) where

import Data.Char 
import Shared

-- encrypts a given String with a given key
encrypt :: [Char] -> [Char] -> [Char]
encrypt ks ps = [chr ((mod ((ord p) + (ord k)) 26)+65) | (k,p) <- zip (prepareKey (prepareText ks) (prepareText ps)) (prepareText ps)]