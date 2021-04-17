
import Data.Char ( isUpper )

oddList :: Int -> Int -> [Int]
oddList y z = [x | x <- [y..z], mod x 2 /= 0]

removeAllUpper :: String -> String
removeAllUpper xs = [x | x <- xs, not (isUpper x)]

union :: Eq a => [a] -> [a] -> [a]
union [] [] = []
union [] (y:ys) = y : union [] ys
union (x:xs) ys
    | elem x ys = union xs ys
    | otherwise = x : union xs ys

intersection :: Eq a => [a] -> [a] -> [a]
intersection [] xs = []
intersection (x:xs) ys
    | elem x ys = x : intersection xs ys
    | otherwise =  intersection xs ys

countChar :: Char -> String -> Int
countChar _ [] = 0
countChar x (y:ys)
    | y == x = 1 + countChar x ys
    | otherwise = countChar x ys

countThem :: String -> [(Char, Int)]
countThem xs = filtered [(x, countChar x xs) | x <- xs]
    where 
        filtered :: [(Char, Int)] -> [(Char, Int)]
        filtered  [] = []
        filtered (x:xs)
            | elem x xs = filtered xs
            | otherwise = x : filtered xs 

isPrime :: Int -> Bool
isPrime n = null [x |x<-[2..ceiling (sqrt (fromIntegral n)::Double)], n `mod` x == 0]

goldbach :: Int-> [(Int, Int)]
goldbach n = let primes = [x |x<-[2..(n `div` 2)+1], isPrime x]
             in [(x,n-x) |x<-primes, isPrime (n-x)]

combinations :: Int -> String -> [String]
combinations 1 xs = [[x]| x<-xs]
combinations n (x:xs) | n == length (x:xs) = [(x:xs)]
                      |otherwise = [[x] ++ y |y<-combinations (n-1) xs ] 
                                    ++ (combinations n xs)


funkce x = x+x

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

allEven xs = xs == [x | x<-xs, even x]

main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")
