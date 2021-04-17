data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
          deriving (Eq)

eval :: Expr -> Int
eval (Num x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Div x y) = div (eval x)  (eval y)

testE1:: Expr
testE1 = Mul (Num 2) $ Add (Num 3) (Num 4)

testE2 :: Expr
testE2 = Add (Mul (Num 2) (Num 3)) (Num 4)

test1 :: Int -> Int -> Int
test1 x y = eval $ Mul (Num x) $ Add (Num x) (Num y)

test2 :: Int -> Int -> Int
test2 x y = eval $ Add (Mul (Num x) (Num x)) (Num y)

test5 :: Expr
test5 = Div (Add (Num 5) (Num 11)) (Num 2)

showExpr expr = showExpr' expr NoOp

data Operation = Hi | HiDiv | Lo | LoSub | NoOp deriving (Eq)

showExpr' :: Expr -> Operation -> String
showExpr' (Num x) _ = show x
showExpr' (Var x) _ =  [x]
showExpr' (Add l r) op = let
  x = showExpr' l Lo ++"+"++showExpr' r Lo
  in if op == Hi || op == HiDiv || op==LoSub 
     then "(" ++ x ++")"
     else x
showExpr' (Sub l r) op = let
  x = showExpr' l Lo ++"-"++showExpr' r LoSub
  in if op == Hi || op == HiDiv || op==LoSub 
     then "(" ++ x ++")"
     else x     
showExpr' (Mul l r) op = let
  x = showExpr' l Hi ++"*"++showExpr' r Hi
  in if op == HiDiv
     then "(" ++ x ++")"
     else x
showExpr' (Div l r) op = let
  x = showExpr' l Hi ++"/"++showExpr' r Hi
  in if op == HiDiv
     then "(" ++ x ++")"
     else x