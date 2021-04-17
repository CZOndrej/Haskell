import Data.Char ( ord, toLower, digitToInt, isLetter )
type Pic = [String]

pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))

board:: Pic
board = (zipWith (++) ["8","7","6","5","4","3","2","1"] (replicate 8 "........")) ++ [" abcdefgh"]

replace :: Int -> String -> Pic -> Pic
replace x c (y:xs)
    | x /= 0 = [y] ++ replace (x-1) c xs
    | otherwise = [c] ++ xs

replaceStr :: Int -> Char -> String -> String 
replaceStr x c (y:xs)
    | x /= 0 = [y] ++ replaceStr (x-1) c xs
    | otherwise = [c] ++ xs

unite :: String -> String -> String
unite [] [] = []
unite (x:xs) (y:ys)
    | x == y = [x] ++ unite xs ys
    | otherwise = if isLetter x then [x] ++ unite xs ys else [y] ++ unite xs ys

unitePic :: Pic -> Pic -> Pic
unitePic [] [] = []
unitePic xs [] = xs
unitePic [] xs = xs
unitePic (x:xs) (y:ys) = [unite x y] ++ unitePic xs ys 
    
rowIndex :: String -> Int
rowIndex xs = 8 - digitToInt (xs!!2)

columnIndex :: String -> Int
columnIndex xs = ord (xs!!1) - 96

newLine :: String -> String
newLine xs = replaceStr (columnIndex xs) (xs!!0) (board!!(rowIndex xs))
     
chess :: [String] -> [String] -> Pic
chess [] [] = []
chess [] (b:bl) = unitePic (replace (rowIndex b) (newLine (strToLower b)) board) (chess [] bl)
chess (w:wh) [] = unitePic (replace (rowIndex w) (newLine w) board) (chess [] wh)
chess (w:wh) (b:bl) =unitePic (helper w b) (chess wh bl) where
    helper :: String -> String -> Pic
    helper w b = unitePic (replace (rowIndex w) (newLine w) board) (replace (rowIndex (b)) (newLine (strToLower b)) board)

strToLower :: String -> String
strToLower [] = []
strToLower (x:xs) = [toLower x] ++ strToLower xs

--Doplnění úkolu



emptyRow::Pic -> Int
emptyRow [_] = 0
emptyRow (x:xs)
    | letters x = 1 + emptyRow xs
    | otherwise = emptyRow xs where
        letters [] = True
        letters (x:xs)
            | isLetter x =  False
            | otherwise = letters xs
