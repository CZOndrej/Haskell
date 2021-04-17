type Pic = [String]

pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))

pic :: Pic
pic = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####"]

flipH :: Pic -> Pic
flipH x = reverse x

flipV :: Pic -> Pic
flipV x = map (reverse) x

above :: Pic -> Pic -> Pic
above x y = x++y

sideBySide :: Pic -> Pic -> Pic
sideBySide x s = zipWith (++) x s

toRow :: String -> Pic
toRow xs = map (\x -> [x]) xs

coloumToRow :: [String] -> String
coloumToRow [] = []
coloumToRow (x:xs) = vystup ++ [x!!0] ++ coloumToRow xs where
    vystup = []

duplicateC :: Int -> a -> [a]
duplicateC 0 _ = []
duplicateC y x = [x] ++ duplicateC (y-1) x

multi :: Int -> String -> String
multi _ [] = []
multi x (y:ys) = duplicateC x y ++ multi x ys 



zoom :: Int -> Pic -> Pic
zoom n xs = [concat(map (duplicateC n) x)|x<-concat (map (duplicateC n) xs)]