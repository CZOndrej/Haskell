
data List a = Null
            | Cons a (List a)
            deriving Show

lst :: List Int
lst = Cons 1 (Cons 2 (Cons 3 Null))

--1. Nadefinujte rekurzivní datovou strukturu Company, která bude obsahovat pojmenované položky:
-- name - název firmy typu String;
-- employees – počet zaměstnanců (Int);
-- ownerOf – seznam dalších firem, které jsou firmou vlastněny.
--Vytvořte příklad takové struktury.

data Company = Company {name :: String, employees :: Int, ownerOf :: [Company] }

comp1 :: Company
comp1 = Company {name = "Hask", employees = 55, ownerOf = []} 

comp2 :: Company
comp2 = Company {name = "Fpr", employees = 32, ownerOf = []} 

comp3 :: Company
comp3 = Company {name = "Own", employees = 52, ownerOf = [comp1, comp2]}

--Nadefinujte rekurzivní datovou strukturu Entity. Instancí tohoto typu je buď:
--Point – definovaný dvěma souřadnicemi typu Double;
--Circle – definovaný středem, což jsou dvě hodnoty Double a poloměrem typu
--Int;
--Container – bude obsahovat několik položek typu Entity, ty jsou uvnitř uloženy ve
--formě seznamu.
--Vytvořte příklad takové struktury

data Entity = Point {x :: Double, y :: Double}
            | Circle {x :: Double, y :: Double, radius :: Int}
            | Container [Entity]

point :: Entity
point = Point {x = 254.6, y = 45.8 }

circle :: Entity
circle = Circle {x = 254.6, y = 45.8, radius = 15 }

cont  :: Entity
cont = Container [circle, point]

--Nadefinujte datovou strukturu Table, popisující strukturu tabulky. Ta je definována jako
--množina sloupců, každý sloupec má jméno (String) a typ hodnoty. To může být jeden
--z následujících typů: Int, Char, String.

data DataType = String
                | Int
                | Char  
data Col = Col {name' :: String, dataType :: DataType } 
newtype Table = Table  [Col]
            

table :: Table
table = Table [Col {name' = "Jmeno", dataType = String}, Col {name' = "Prijmeni", dataType = String}]


data Tag = Tag {tagName :: String, attributes :: [Attribute], innner :: [Tag]}
data Attribute = Attribute {name''' :: String, value :: String}
newtype HTMLDocument = HTMLDocument [Tag]

doc :: HTMLDocument
doc = HTMLDocument [Tag {tagName = "html", attributes = [], innner = [(Tag {tagName ="head", attributes = [Attribute {name''' = "fontsize", value = "Bold"}], innner = []})]}]


data FileType = Image | Executable | SourceCode | TextFile
data Entry = File {name'' :: String, size :: Int, ftype :: FileType}
 | Directory {name'' :: String, entries :: [Entry]}



root :: Entry
root = Directory "root"
 [
 File "logo.jpg" 5000 Image,

 Directory "classes"
    [
    File "notes-fpr.txt" 200 TextFile,
    File "presentation.jpg" 150 Image,
    File "first_test.hs" 20 SourceCode
    ],
 File "logo.jpg" 5000 Image
 ] 

--Napište funkci, která spočítá a vrátí počet souborů v adresářové struktuře předané jako parametr
--(tedy i ve všech podadresářích).
--countFiles :: Entry -> Int

instance Eq Entry where
   File {} == File {} = True
   Directory {} == Directory {} = True
   _ == _ = False

countFiles :: Entry -> Int
countFiles File {} = 1
countFiles (Directory _ []) = 0
countFiles (Directory  x (f:xs)) = countFiles (Directory x xs) + countFiles f


--Napište funkci, která vrátí počet obrázků (FileType Image) v adresářové struktuře, předané
--jako parametr (tedy i ve všech podadresářích).
--countImages :: Entry -> Int

countImages :: Entry -> Int
countImages File {name'' = _, size = _, ftype = Image} = 1
countImages File {} = 0
countImages (Directory _ []) = 0
countImages (Directory  x (f:xs)) = countImages (Directory x xs) + countImages f
  

--Napište funkci, která spočítá a vrátí počet adresářů v adresářové struktuře, předané jako
--parametr (tedy i ve všech podadresářích).
--countDirectories :: Entry -> Int

countDirectories :: Entry -> Int
countDirectories File {} = 0
countDirectories (Directory _ []) = 1
countDirectories (Directory  x (f:xs))
   | isDir f = countDirectories (Directory x xs) + countDirectories f
   | otherwise = countDirectories (Directory x xs)

isDir :: Entry -> Bool
isDir (Directory _ _) = True
isDir _ = False


--Napište funkci, která spočítá a vrátí celkovou velikost všech souborů v adresářové struktuře,
--předané jako parametr (tedy i ve všech podadresářích).
--countSize :: Entry -> Int

countSize :: Entry -> Int
countSize (File _ x _) = x
countSize (Directory _ []) = 0
countSize (Directory  x (f:xs)) = countSize f + countSize (Directory x xs)
   
--Napište funkci, která spočítá a vrátí počet souborů, které jsou větší než zadaný limit. Ten je stejně
--jako adresářová struktura, předán jako parametr.
--countLargerFiles :: Int -> Entry -> Int

countLargerFiles :: Int -> Entry -> Int
countLargerFiles s (File _ x _) = if x>s then 1 else 0
countLargerFiles _ (Directory _ []) = 0
countLargerFiles s (Directory  x (f:xs)) = countLargerFiles s f + countLargerFiles s (Directory x xs)

--Napište funkci, která pro všechny soubory z předané struktury vrátí jejich plný název včetně
--cesty. Tyto položky budou vráceny jako seznam. Ve výsledných položkách, každý název
--adresáře a souboru začíná znakem „/“ (jako na systémech Linux).
--fullNames:: Entry -> [String]
   
   
fullNames :: Entry -> [String]
fullNames (File n _ _) = ["/"++n]
fullNames (Directory _ []) = []
fullNames (Directory n (x:xs))
   | isFile x = ["/" ++ n ++ "/" ++ (fileName x) ] ++ fullNames (Directory n xs)
   | otherwise =zipWith' n (fullNames x) ++  fullNames (Directory n xs)

isFile :: Entry -> Bool
isFile File {} = True
isFile _ = False


fileName :: Entry -> String
fileName (File n _ _) = n

zipWith' :: String -> [String] -> [String]
zipWith' x (y:ys) = [x++y] ++ ys

directorySizes :: Entry -> [(String, Int)]
directorySizes File {} = []
directorySizes x= [(,)] 


   






    
