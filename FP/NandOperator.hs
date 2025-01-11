module NandOperator where

(!&) :: Bool -> Bool -> Bool
x !& y = not (x && y)
