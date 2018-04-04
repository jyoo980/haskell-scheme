module Eval where
import Data

-- val@(String _) matches against any LispVal that's 
-- a string and then binds val to the whole LispVal
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Fractional _) = val
eval val@(Bool _) = val
eval val@(List [Atom "quote", val]) = val