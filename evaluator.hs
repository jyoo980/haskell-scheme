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
              ("remainder", numericBinop rem),
              ("string?", unaryOp stringp),
              ("bool?", unaryOp boolp),
              ("number?", unaryOp numberp),
              ("list?", unaryOp listp),
              ("true?", unaryOp truep),
              ("false?", unaryOp falsep),
              ("not", unaryOp notp),
              ("and", booleanBinop(&&)),
              ("or", booleanBinop(||)),
              ("xor", booleanBinop xor)]

xor :: Bool -> Bool -> Bool 
xor a b = a /= b

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

booleanBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> LispVal
booleanBinop op params = Bool $ foldl1 op $ map unpackBool params
              
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b
unpackBool _ = False

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [val] = f val

stringp, boolp, numberp, listp :: LispVal -> LispVal 
stringp (String _)  = Bool True
stringp _           = Bool False
boolp (Bool _)      = Bool True
boolp _             = Bool False
numberp (Number _)  = Bool True
numberp _           = Bool False
listp (List _)      = Bool True
listp _             = Bool False
truep (Bool True)   = Bool True
truep (Bool _)      = Bool False
falsep (Bool False) = Bool True
falsep (Bool _)     = Bool False
notp (Bool True)    = Bool False
notp (Bool _)       = Bool True