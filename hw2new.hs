
type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

data Point = Point Int Int
data Shape = Circle Point Int
           | Rectangle {topLeft:: Point, bottomRight::Point}

instance Eq Shape where
    Circle _ _ == Circle _ _ = True
    Rectangle _ _ == Rectangle _ _ = True
    _ == _ = False

view :: (Int,Int) -> [Shape] -> Result
view _ [] = []
view (cols,rows) (x:xs)
 | x == Rectangle (Point 0 0) (Point 0 0) = uniteResults (replace (coorRec x) 0 (genBoard (cols,rows))) (view (cols,rows) xs)
 | otherwise = uniteResults (replace (coorCirc x) 0 (genBoard (cols,rows))) (view (cols,rows) xs) where
    
    uniteResults :: Result -> Result-> Result
    uniteResults x [] = x
    uniteResults (a:resta) (b:restb) = [uniteRow a b] ++ uniteResults resta restb where
        uniteRow _ [] = []
        uniteRow (x:xs) (y:ys)
            | x == y = [x] ++ uniteRow xs ys
            | otherwise = if x == '#' then [x] ++ uniteRow xs ys else [y] ++ uniteRow xs ys
    
    coorRec :: Shape -> [(Int,Int)]
    coorRec (Rectangle (Point x y) (Point a b)) = [(m,y) | m<-[x..a]] ++ [(m,n) | m <- [x,a], n <- [(y+1)..(b-1)]] ++ [(m,b) | m <- [x..a]]
    
    genBoard:: (Int,Int) -> Result
    genBoard (x,y) = replicate y (replicate x '.')
    
    replace :: [(Int,Int)] -> Int-> Result -> Result
    replace _ _ [] = []
    replace doubles row (x:xs) = [string] ++ replace doubles (row+1) xs where
        string = replaceRow row 0 x doubles where
            
            replaceRow :: Int -> Int -> String ->  [(Int,Int)] -> String
            replaceRow _ _ [] _ = []
            replaceRow row col (x:xs) doubles
                | (col, row) `elem` doubles = "#" ++ replaceRow row (col+1) xs doubles
                | otherwise = "." ++ replaceRow row (col+1) xs doubles
    
    coorCirc :: Shape -> [(Int,Int)]
    coorCirc (Circle (Point x y) r) = circCoor'' (circCoor' (circCoor 0 r (3-(2*r)))) (x,y) where
        
        circCoor :: Int-> Int -> Int -> [(Int,Int)]
        circCoor x y d
            | x<=y = if d<0 then [(x,y)] ++ circCoor (x+1) y (d+(4*(x+1)+6)) else [(x,y)] ++ circCoor (x+1) (y-1) (d+4*x+10)
            | otherwise = []
        
        circCoor' :: [(Int,Int)] -> [(Int,Int)]
        circCoor' [] = []
        circCoor' ((x,y):rest) = [(x,y)] ++ [(-x,y)] ++ [(x,-y)] ++ [(-x,-y)] ++ [(y,x)] ++ [(-y,x)] ++ [(y,-x)] ++ [( -y,-x)] ++ circCoor' rest
        
        circCoor'' :: [(Int,Int)] -> (Int,Int) ->[(Int,Int)]
        circCoor'' [] _ = []
        circCoor'' ((x,y):rest) (a,b) = [((x+a),(y+b))] ++ circCoor'' rest (a,b)
    




 
