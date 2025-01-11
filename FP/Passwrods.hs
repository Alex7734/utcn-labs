module Password where

allChars :: String 
allChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

passwords :: Int -> [String]
passwords n = sequenceA (replicate n allChars)