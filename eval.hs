module Eval where
import Data

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\'"
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Fractional frac) = show frac
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (DottedList head tail) =
    "(" ++ unwordList head ++ "." ++ showVal tail ++ ")" 

unwordList :: [LispVal] -> String
unwordList = unwords . map showVal