import System.Environment (getArgs)
import Data.Char (chr, ord, isLetter, toLower)
import Data.List (tails)  

shiftChar :: Int -> Char -> Char
shiftChar shift c 
  | isLetter c = 
      let a = if c >= 'A' && c <= 'Z' then 'A' else 'a'
      in chr $ (ord c - ord a - shift + 26) `mod` 26 + ord a
  | otherwise = c

decrypt :: Int -> String -> String
decrypt shift = map (shiftChar shift)

allDecryptions :: String -> [(Int, String)]
allDecryptions ciphertext = [(key, decrypt key ciphertext) | key <- [0..25]]

contains :: String -> String -> Bool
contains needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

crackCaesar :: String -> String -> Maybe (Int, String)
crackCaesar knownWord ciphertext = 
  case filter (\(_, plaintext) -> contains (map toLower knownWord) (map toLower plaintext)) (allDecryptions ciphertext) of
    ((key, plaintext):_) -> Just (key, plaintext)
    [] -> Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [knownWord] -> do
      ciphertext <- getContents
      case crackCaesar knownWord (filter (/= '\n') ciphertext) of
        Just (key, plaintext) -> do
          putStrLn $ "Found plaintext with key: " ++ show key
          putStrLn plaintext
        Nothing -> putStrLn "Failed to crack cipher"
    _ -> putStrLn "Usage: runhaskell CrackCaesar.hs <knownWord>"
