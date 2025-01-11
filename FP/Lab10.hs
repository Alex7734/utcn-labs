module Lab10 where

--------------------------------------------
-- Functors
--------------------------------------------

-- fuctors should respect the functor laws:
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g

-- The more common form of fmap is it's infix version <$>
-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b

fmap :: (Functor f) => (a -> b) -> f a -> f b
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

-- fmap (+1) (Just 1) = Just 2
-- fmap (+1) [1, 2, 3] = [2, 3, 4]
-- fmap ("hello " ++) (Just "world") = Just "hello world"

--------------------------------------------
--- Applicatives
--------------------------------------------

-- Applicatives are functors that have an additional function called pure
-- pure :: a -> f a
-- pure takes a value and wraps it in a functor

-- Applicatives also have an infix operator <*>
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

-- use pure to directly inject a function into the applicative
-- use fmap to apply a function that is already in the applicative

-- The Maybe applicative
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

-- (\x y z -> x*100 + y*10 + z) <$> Just 1 <*> Just 2 <*> Just 3
-- Just 123
-- pure (+) <*> Just 1 <*> Just 1
-- Just 2
-- (fmap (+) (Just 1)) <*> Just 1
-- Just 2

-- The List applicative
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

--------------------------------------------
--- Monads
--------------------------------------------

-- The goal of Monads is to deal with actions or side effects
-- They introduce the bind operator >>=

-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- it allows us to chain actions together
-- it defines the return function which is the equivalent of pure in applicatives

-- The Maybe monad
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    (Just x) >>= f = f x

-- half x = if even x then Just (x `div` 2) else Nothing
-- half 3 --> Nothing
-- half 4 --> Just 2
-- (half 4) >>== half --> Just 1
-- (half 3) >>== half --> Nothing

-- With the bind operator (>>=) we can easily and elegantly chain operations 
-- that can fail, writing code that looks like it only handles the “happy path”.
-- If an error occurs ( Nothing is returned), it is propagated forward 
-- and can be handled at the end of the chain.

-- The List monad
instance Monad [] where
    return = pure
    xs >>= f = concat (map f xs)

-- The IO monad
-- The IO monad is a special case because it is the only way to perform I/O in Haskell.

main :: IO ()
main = putStrLn "Please enter your name:" >>==
\_ -> getLine >>= (\x -> putStrLn ("Hello, " ++ x))

-- or we can write it as:

main :: IO ()
main = putStrLn "Please enter your name:" >>
getLine >>= (\x -> putStrLn ("Hello, " ++ x))

-- The >> operator is a monadic operator that discards the result of the first action
-- and returns the result of the second action.

-- If you would have a more complex program that requires multiple IO actions,
-- you can use the do notation to make the code more readable.

main :: IO ()
main = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name)

-- A more complex example would be:

main :: IO ()
main = do 
    putStrLn "Choose a mode: toUpper or toLower"
    mode <- getLine
    if mode == "toUpper" then do
        putStrLn "Please enter a string:"
        str <- getLine
        putStrLn (map toUpper str)
    else if mode == "toLower" then do
        putStrLn "Please enter a string:"
        str <- getLine
        putStrLn (map toLower str)
    else
        putStrLn "Invalid mode"


--------------------------------------------
--- Working with files and command line arguments
--------------------------------------------

main :: IO ()
main = do
    nrStr:args <- getArgs
    let nr = read nrStr :: Maybe Int
    case nr of 
        Nothing -> putStrLn "First argument must be a positive number"
        Just n 
            | n < 0 -> putStrLn "First argument must be a positive number"
            | otherwise ->
                case compare (length args) n of
                    LT -> putStrLn "Not enough arguments"
                    EQ -> putStrLn "Just enough arguments"
                    GT -> putStrLn "Too many arguments"

