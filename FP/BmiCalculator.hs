import Text.Read (readMaybe)
import System.Environment (getArgs)

data Result err ok = Err err | Ok ok
  deriving (Show, Eq)

instance Functor (Result err) where
  fmap _ (Err e) = Err e
  fmap f (Ok x)  = Ok (f x)

instance Applicative (Result err) where
  pure = Ok
  (Err e) <*> _       = Err e
  _ <*> (Err e)       = Err e
  (Ok f) <*> (Ok x)   = Ok (f x)

instance Monad (Result err) where
  (Err e) >>= _ = Err e
  (Ok x)  >>= f = f x

validateWeight :: String -> Result String Float
validateWeight input = case readMaybe input :: Maybe Float of
  Nothing -> Err "Error: Invalid weight"
  Just w  -> if w >= 1 && w <= 300
                then Ok w
                else Err "Error: Invalid weight"

validateHeight :: String -> Result String Float
validateHeight input = case readMaybe input :: Maybe Float of
  Nothing -> Err "Error: Invalid height"
  Just h  -> if h >= 0.2 && h <= 2.5
                then Ok h
                else Err "Error: Invalid height"

computeBmi :: Float -> Float -> Float
computeBmi weight height = weight / (height * height)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [weightInput, heightInput] -> do
      let result = do
            weight <- validateWeight weightInput
            height <- validateHeight heightInput
            return (computeBmi weight height)
      case result of
        Err errorMsg -> putStrLn errorMsg
        Ok bmi       -> print bmi
    _ -> putStrLn "Usage: runhaskell BmiCalculator.hs <weight> <height>"