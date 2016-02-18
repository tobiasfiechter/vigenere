module Decrypt
(decrypt) where

import Data.Char 
import Shared

-- decrypts a given String with a given key
decrypt :: [Char] -> [Char] -> [Char]
decrypt ks cs = [chr ((mod ((ord p) - (ord k)) 26)+65) | (k,p) <- zip (prepareKey (prepareText ks) (prepareText cs)) (prepareText cs)]