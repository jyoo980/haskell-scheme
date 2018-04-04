module Data where

-- Defining data our parser will return our parsed values as
data LispVal = Atom String
    | List[LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Fractional Double
    | String String
    | Bool Bool