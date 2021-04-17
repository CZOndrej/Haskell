
--Nadefinujte rekurzivní datovou strukturu Company, která bude obsahovat pojmenované položky:
--name-název firmytypu String;
--employees–početzaměstnanců(Int);
--ownerOf–seznamdalších firem, které jsou firmou vlastněny.

data Company = Company {name :: String, employees :: Int, ownerOf :: [Company] }

comp1 :: Company
comp1 = Company {name = "Hask", employees = 55, ownerOf = []} 

comp2 :: Company
comp2 = Company {name = "Fpr", employees = 32, ownerOf = []} 

comp3 :: Company
comp3 = Company {name = "Own", employees = 52, ownerOf = [comp1, comp2]}


--------------------------------------------------------------------
data FileType=Image|Executable|SourceCode|TextFile
data Entry = File {name'::String, size::Int, ftype:: FileType}
            |Directory {name'::String,entries::[Entry]}
            
root::Entry
root=Directory"root"
    [
    File"logo.jpg"5000Image,
    Directory"classes"
        [
        File"notes-fpr.txt"200TextFile,
        File"presentation.jpg"150Image,
        File"first_test.hs"20SourceCode
        ],
        File"presentation.jpg"150Image
    ]

----------------------------------------------------------------------

--Napište funkci, která spočítá a vrátí počet adresářů v adresářové struktuře,předané jako parametr (tedy i ve všech podadresářích).

countDirectories :: Entry -> Int
countDirectories File {} = 0
countDirectories (Directory _ []) = 1
countDirectories (Directory  x (f:xs))
   | isDir f = countDirectories (Directory x xs) + countDirectories f
   | otherwise = countDirectories (Directory x xs) where
        isDir :: Entry -> Bool
        isDir (Directory _ _) = True
        isDir _ = False

--Napište funkci, která vrátí jména všech souborů z předané adresářové struktury, která mají stejnou příponu jako zadaný parametr.

returnSuffix :: Entry -> String
returnSuffix (File (n:ns) _ _)
    | n == '.' = ns
    | otherwise = returnSuffix (File {name' = ns, size = 0, ftype = Image})


getFiles :: Entry -> String -> [String]
getFiles f@(File n _ _) suf = if suf == returnSuffix f then [n] else []
getFiles (Directory _ []) _ = []
getFiles (Directory _ (x:xs)) suf = getFiles x suf ++ getFiles (Directory "" xs) suf
    
    


