module Evaluator where
import Data

-- val@(String _) matches against any LispVal that's 
-- a string and then binds val to the whole LispVal
-- map eval args -> evaluates args recursively, applies func to them
-- func : args == (fn a1 a2....)
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Fractional _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- Maybe will produce false if the primitive is not found, else
-- func will be evaluated on args ($ args)
apply :: String -> [LispVal] -> LispVal
apply func args = 
    maybe (Bool False) ($ args) $ lookup func primitives

-- Our primitives store, pairs are Strings representing the actual primitives to
-- the functions themselves (StringRepresentingPrimitive, ActualPrimitive)
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop(+)),
              ("-", numericBinop(-)),
              ("*", numericBinop(*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
              
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
    if null parsed
        then 0
        else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0




