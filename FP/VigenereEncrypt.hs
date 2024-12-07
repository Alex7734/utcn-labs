import System.Environment (getArgs)
import Data.Char (chr, ord, isLetter, toUpper)

encryptChar :: Char -> Char -> Char
encryptChar keyChar plainChar 
  | isLetter plainChar = 
      let shift = ord (toUpper keyChar) - ord 'A'
          base = if plainChar >= 'A' && plainChar <= 'Z' then 'A' else 'a'
      in chr $ (ord plainChar - ord base + shift) `mod` 26 + ord base
  | otherwise = plainChar  

vigenereEncrypt :: String -> String -> String
vigenereEncrypt key message = zipWith encryptChar (cycle (map toUpper key)) message

main :: IO ()
main = do
  args <- getArgs
  case args of
    [keyFilePath] -> do
      key <- readFile keyFilePath
      plaintext <- getContents
      let encrypted = vigenereEncrypt (filter isLetter key) plaintext
      putStrLn encrypted
    _ -> putStrLn "Usage: runhaskell VigenereEncrypt.hs <keyfile>"
