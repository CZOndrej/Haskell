import Data.Text

--Napište funkci, která vrátí symboly na sudých pozicích v řetězci.

evens::String->String
evens [] = []
evens [_] = []
evens (x:y:xs) = [y] ++ evens xs

--Mějme seznam čísel typu Double, spočítejte průměr z těchto čísel.
--average::[Double]->Double
--average [1,2,3,4]--2.5

average::[Double]->Double
average xs =  sum xs / len xs

len :: [a] -> Double
len [] = 0
len (x:xs) = 1 + len xs

len' :: String -> Int
len' [] = 0
len' (x:xs) = 1 + len' xs
--Mějme na vstupu seznam dvojic (Jmeno, Prijmeni) a řetězec vzor, vraťte jména všech,
--jejichž příjmení obsahuje zadaný vzor.



names::[(String,String)]->String->[String]
names [] [] = []
names _ [] = []
names [] _ = []
names ((y,s):ys) xs
    | con s xs == len' xs = [y] ++ (names ys xs)
    | otherwise = names ys xs

con :: String -> String -> Int
con _ [] = 0
con [] _ = 0
con (x:xs) (y:ys)
    | x == y = 1 + con xs ys
    | otherwise = con xs (y:ys)
