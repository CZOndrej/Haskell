type Pic = [String]
pic :: Pic
pic = [ "8........",
        "7........",
        "6........",
        "5........",
        "4........",
        "3........",
        "2........",
        "1........",
        " abcdefgh"]

pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))



