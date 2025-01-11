module Lab8 where 

sudan :: Int -> Int -> Int -> Int
sudan n x y 
  | n == 0 = x + y
  | n > 0 && y == 0 = x
  | otherwise = let subResult = sudan n x (y-1)
                in sudan (n-1) subResult (y + subResult)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

average :: [Int] -> Float
average [] = 0
average xs = realToFrac (sum xs) / realToFrac (length xs)

countVowels :: String -> Int
countVowels str = length [ c | c <- str, c `elem` "aeiouAEIOU" ]

addBigs :: [Int] -> [Int] -> [Int]
addBigs xs ys = reverse (addWithCarry (reverse xs) (reverse ys) 0)
  where addWithCarry [] [] carry = if carry == 0 then [] else [carry]
        addWithCarry [] ys carry = addWithCarry [0] ys carry
        addWithCarry xs [] carry = addWithCarry xs [0] carry
        addWithCarry (x:xs) (y:ys) carry = let sum = x + y + carry
                                           in sum `mod` 10 : addWithCarry xs ys (sum `div` 10)

breakToLines :: Int -> String -> [String]
breakToLines lineLen str 
  | length str <= lineLen = [str]
  | otherwise = take lineLen str : breakToLines lineLen (drop lineLen str)

formatLines :: [String] -> String
formatLines [] = ""
formatLines [x] = x
formatLines (x:xs) = x ++ "\n" ++ formatLines xs

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

formattedFact :: Integer -> Int -> String
formattedFact n lineLen = formatLines (breakToLines lineLen (show (fact n)))
