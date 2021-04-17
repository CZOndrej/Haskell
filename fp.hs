import Prelude

sumIt::[Int]->Int
sumIt [] = 0
sumIt (x:xs)
    | mod x 2 == 0 = x + sumIt xs
    | otherwise = sumIt xs

extract :: [Char] -> Int
extract [] = 0
extract (x:xs)
    | x >= 'a' && x <= 'z' = 1 + extract xs
    | otherwise = extract xs

countIt :: Int-> [Int] -> Int
countIt _ [] = 0
countIt x (y:ys)
    | x == y = 1 + countIt x ys
    | otherwise = countIt x ys

sumPossitive::[Int]->Int
sumPossitive [] = 0
sumPossitive (x:xs)
    | x > 0 = x + sumPossitive xs
    | otherwise = sumPossitive xs

average::[Int] -> Double
average xs = fromIntegral (sum' xs) / fromIntegral (length' xs)  where 
    sum' [] = 0
    sum' (x:xs) = x + sum' xs
    length' [] = 0
    length' (x:xs) = 1 + length' xs

palindrom::String -> Bool
palindrom xs
    | reverse' xs == xs = True
    | otherwise = False
    where
        reverse' [] = []
        reverse' (x:xs) = (reverse xs) ++ [x]

removeThem::String->String
removeThem [] = []
removeThem (x:xs)
    | x >= 'A' && x <= 'Z'= '_' : removeThem xs
    | otherwise = x : removeThem xs

countPossitive::[Int]->Int
countPossitive [] = 0
countPossitive (x:xs)
    | x > 0 = 1 + countPossitive xs
    | otherwise = countPossitive xs

sumIt'::[Int]->Int
sumIt' [] = 0
sumIt' (x:xs)
    | mod x 2 /= 0 = x + sumIt xs
    | otherwise = sumIt xs

countIt' :: Char-> String -> Int
countIt' _ [] = 0
countIt' x (y:ys)
    | x == y = 1 + countIt' x ys
    | otherwise = countIt' x ys

countDigits :: String -> Int
countDigits [] = 0
countDigits (y:ys)
    | y >= '0' && y <= '9' = 1 + countDigits ys
    | otherwise = countDigits ys

countZeros :: [Int] -> Int
countZeros [] = 0
countZeros (x:xs)
    | x == 0 = 1 + countZeros xs
    | otherwise = countZeros xs

primes :: [Int] -> Int
primes [] = 0
primes (x:xs)
    | isPrime x = 1 + primes xs
    | otherwise = primes xs
    where
        isPrime x = helper x x where
            helper _ 2 = True
            helper x y
                | mod x (y-1) == 0 = False
                | otherwise = helper x (y-1)

getThem :: [Int] -> String -> String
getThem [] xs = []
getThem (x:xs) zs = index x zs : getThem xs zs where
    index :: Int -> String -> Char
    index 1 (y:ys) = y 
    index x (y:ys) = index (x-1) ys 


max' :: [Int] -> [Int]
max' [x,y] = []
max' (x:y:z:xs)
    | y > x && y > z = y : max' (y:z:xs)
    | otherwise = max' (y:z:xs)


fib' :: [Int] -> [Int]
fib' [x,y] = []
fib' (x:y:z:xs)
    | x + y == z = z : fib' (y:z:xs)
    | otherwise = fib' (y:z:xs)


sumIt'' :: [Int] -> [Int] -> Int
sumIt'' [] xs = 0
sumIt'' (x:xs) ys = index x ys + sumIt'' xs ys where
    index :: Int -> [Int] -> Int
    index 1 (y:ys) = y
    index x (y:ys) = index (x-1) ys  


countDiff :: String -> String -> Int
countDiff [] [] = 0
countDiff [] (x:xs) = 1 + countDiff [] xs
countDiff (x:xs) [] = 1 + countDiff xs []
countDiff (x:xs) (y:ys)
    | x == y = countDiff xs ys
    | otherwise = 1 + countDiff xs ys


dividers :: [Int] -> [Int]
dividers [] = []
dividers (x:xs) = numOfDiv x x: dividers xs where
    numOfDiv x 0 = 0
    numOfDiv x y
        | mod x y == 0 = 1 + numOfDiv x (y-1)
        | otherwise = numOfDiv x (y-1)


ordered :: [Int] -> Bool
ordered [x] = True
ordered (x:y:xs)
    | x < y = ordered (y:xs)
    |otherwise = False
   

withoutPrimes :: [Int] -> [Int]
withoutPrimes [] = []
withoutPrimes (x:xs)
    | isPrime x = withoutPrimes xs
    | otherwise = x : withoutPrimes xs
    where
        isPrime x = helper x x where
            helper _ 2 = True
            helper x y
                | mod x (y-1) == 0 = False
                | otherwise = helper x (y-1)

