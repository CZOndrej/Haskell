not' :: Bool -> Bool
not' True = False
not' False = True
infixl 5 `not'` 

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False
infixl 4 `and'` 

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True
infixl 3 `or'` 

nand' :: Bool -> Bool -> Bool
nand' x y = not' (and' x y)
infixl 4 `nand'` 

xor' :: Bool -> Bool -> Bool
xor' x y = x/=y
infixl 3 `xor'` 

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True
infixl 2 `impl'` 

equ' :: Bool -> Bool -> Bool
equ' x y = x == y
infixl 7 `equ'` 

table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStr(concat [nicePrint (x:y:xs) ++ " " ++ show (f x y) ++ "\n" | (x:y:xs) <- values]) where
    values = [x:y:[] | x <- [True, False], y <- [True, False]]


tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = putStr(concat [nicePrint x ++ " => " ++ show(f x) ++ "\n" |x<-allValues n]) where 
  allValues 1 = [[True], [False]]
  allValues n = [x:y| x<-[True,False], y<-allValues (n-1)]

nicePrint :: [Bool] -> String
nicePrint xs = concat [show x++"\t"| x<-xs]









    


