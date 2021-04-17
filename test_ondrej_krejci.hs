--Vytvořte funkci, která dostane seznam čísel. 
--Pro tato čísla spočítá průměrnou hodnotu.
--Pro převod Intna Doublelze použít funkci fromIntegral.average::[Int]->Double--average[1,2,3,4,5]=3.0

average::[Int] -> Double
average xs = fromIntegral (sum' xs) / fromIntegral (length' xs)  where 
    sum' [] = 0
    sum' (x:xs) = x + sum' xs
    length' [] = 0
    length' (x:xs) = 1 + length' xs


--Vytvořte funkci, která dostane seznam čísel a vrátí všechna čísla, která jsou součtem dvou předcházejících.
--Čísla, která nemají alespoň dva předcházející členy neberte vpotaz.fib'::[Int]->[Int]--fib'[1,2,3,2,3,5]=[3,5]

fib' :: [Int] -> [Int]
fib' [x,y] = []
fib' (x:y:z:xs)
    | x + y == z = z : fib' (y:z:xs)
    | otherwise = fib' (y:z:xs)