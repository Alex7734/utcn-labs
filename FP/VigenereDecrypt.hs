import System.Environment (getArgs)
import Data.Char (chr, ord, isLetter, toUpper)

decryptChar :: Char -> Char -> Char
decryptChar keyChar cipherChar 
  | isLetter cipherChar = 
      let shift = ord (toUpper keyChar) - ord 'A'
          base = if cipherChar >= 'A' && cipherChar <= 'Z' then 'A' else 'a'
      in chr $ (ord cipherChar - ord base - shift + 26) `mod` 26 + ord base
  | otherwise = cipherChar 

vigenereDecrypt :: String -> String -> String
vigenereDecrypt key message = zipWith decryptChar (cycle (map toUpper key)) message

main :: IO ()
main = do
  args <- getArgs
  case args of
    [keyFilePath] -> do
      key <- readFile keyFilePath
      ciphertext <- getContents
      let decrypted = vigenereDecrypt (filter isLetter key) ciphertext
      putStrLn decrypted
    _ -> putStrLn "Usage: runhaskell VigenereDecrypt.hs <keyfile>"
