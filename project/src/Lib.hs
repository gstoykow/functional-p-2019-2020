module Lib where

    import qualified Data.Text as T
    import qualified Data.Map as M
    
    type Name = String
    type Path = String
    type FullPath = String
    type Content = String
    
    type FS = (M.Map FullPath [Name], M.Map FullPath Content)
    type FSState = (FS, [Name])
    
    dirs :: FS -> M.Map FullPath [Name]
    dirs = fst
    
    files :: FS -> M.Map Name Content
    files = snd
    
    fsys :: FSState -> FS
    fsys = fst
    
    cpath :: FSState -> [Name]
    cpath = snd
    
    createFullPath :: Path -> Name -> FullPath
    createFullPath p n =  p ++ "/" ++ n
    
    buildPath :: [Name] -> Path
    buildPath [] = []
    buildPath (x:xs)
       | x == "" = x ++ buildPath xs
       | otherwise = "/" ++ x ++ buildPath xs
    
    takeNameFromPath :: FullPath -> Name
    takeNameFromPath path = reverse (takeWhile (/= '/') (reverse path))
    
    pathToArr :: FullPath -> [String]
    pathToArr path = map T.unpack (T.splitOn (T.pack "/") (T.pack path))
    
    parseString :: String -> [String]
    parseString line = map T.unpack (T.splitOn (T.pack " ") (T.pack line))
    
    getDirContent :: Maybe [Name] -> Name
    getDirContent Nothing = []
    getDirContent (Just []) = []
    getDirContent (Just (x:xs)) 
       | length (x:xs) > 1 = x ++ " " ++ getDirContent (Just xs)
       | otherwise = x ++ getDirContent (Just xs)
    
    initroot :: (M.Map String [a1], M.Map k a2)
    initroot = (M.insert "" [] M.empty, M.empty)

    
    mkdir :: FS -> Path -> Name -> FS
    mkdir fs path name 
       | not (M.member path (dirs fs)) || M.member (createFullPath path name) (dirs fs) = fs
       | otherwise = (M.insert (createFullPath path name) [] (M.insertWith (++) path [name] (dirs fs)), files fs)

    touch :: FS -> Path -> Name -> Content -> FS
    touch fs path name cont
       | M.member (createFullPath path name) (files fs)|| not (M.member path (dirs fs)) = fs
       | otherwise = (M.insertWith (++) path [name] (dirs fs), M.insert (createFullPath path name) cont (files fs))
    
    findFile :: FS -> FullPath -> (FullPath, Maybe String)
    findFile fs fullpath  
       | M.member fullpath (files fs) = (fullpath, M.lookup fullpath (files fs))
       | otherwise = ("-", M.lookup fullpath (files fs))
    
    findDir :: FS -> FullPath -> (FullPath, Maybe [Name])
    findDir fs fullpath
       | M.member fullpath (dirs fs) = (fullpath, M.lookup fullpath (dirs fs))
       | otherwise = ("-", M.lookup fullpath (dirs fs))
    
    findParenth :: [Name] -> Path
    findParenth [] = []
    findParenth (x:xs)  
        | null xs = []
        | otherwise =  x ++ "/" ++ findParenth xs 
    
    findParent :: [Name] -> Path
    findParent lst = helper (findParenth lst)
      where helper = reverse . drop 1 . reverse
             
    
    remFileFromDir :: Name -> [Name] -> [Name]
    remFileFromDir [] dir = dir
    remFileFromDir _ [] = []
    remFileFromDir name (x:xs)
        | name == x = remFileFromDir name xs
        | otherwise = x : remFileFromDir name xs 
        
    removeFile :: FS -> FullPath -> FS
    removeFile fs [] = fs
    removeFile fs fullpath
       | not (M.member fullpath (files fs)) = fs
       | otherwise = (M.update (Just . remFileFromDir (func (reverse (pathToArr fullpath)) ))  (findParent (pathToArr fullpath)) (dirs fs), M.delete fullpath (files fs))
         where func [] = error "Empty"
               func (x:_)=  x
    
    normalizePath :: [Name] -> FSState -> [Name]
    normalizePath [] _ = []
    normalizePath (x:xs) fsstate = foldl helper [] normalized
           where normalized
                   | x == "" = xs
                   | otherwise = cpath fsstate ++ (x : xs)
                 helper total curr
                   | curr == "." || curr == "" = total
                   | curr == ".." && total /= [] = fst (splitAt (length total - 1) total)
                   | curr == ".." && null total = []
                   | otherwise = total ++ [curr]
    
    command :: FSState -> [String] -> (FSState, String)
    command fsstate [] = (fsstate,"")
    command fsstate (x:[])
       | x == "ls"  = ls fsstate Nothing
       | x == "pwd" = pwd fsstate
    command fsstate (x:y:xs)
       | x == "cd" = cd fsstate y
       | x == "ls" = ls fsstate (Just y)
       | x == "cat" = cat fsstate (y:xs)
       | x == "rm" = rm fsstate (y:xs)
       | otherwise = pwd fsstate

    pwd :: FSState -> (FSState, String)
    pwd fsstate = (fsstate, buildPath (cpath fsstate))

    cd :: FSState -> Path -> (FSState, String)
    cd fsstate pth = ((fsys fsstate, normalizePath (pathToArr pth) fsstate), "")

    ls :: FSState -> Maybe Path -> (FSState, String)
    ls fsstate Nothing = (fsstate, getDirContent (snd (findDir (fsys fsstate) (buildPath (cpath fsstate))))) 
    ls fsstate (Just pth) = (fsstate, getDirContent (snd (findDir (fsys fsstate) (buildPath (normalizePath (pathToArr pth) fsstate)))))

    dropIt :: [a] -> [a]
    dropIt = reverse . drop 2 . reverse

    cat :: FSState -> [FullPath] -> (FSState, String)
    cat fsstate [] = (fsstate,"")
    cat fsstate paths@(x:_) 
       | length paths == 1 = (fsstate, mapper (snd (findFile (fsys fsstate) (buildPath (normalizePath (pathToArr x) fsstate)))))
       | (paths !! (length paths - 2)) /= ">" = (fsstate, concatMap (\fp ->  mapper (snd (findFile (fsys fsstate) (buildPath (normalizePath (pathToArr fp) fsstate))))  ) paths)
       | otherwise = ((touch (fsys fsstate) 
       (findParent (pathToArr(paths !! (length paths - 1)))) 
       (takeNameFromPath (paths !! (length paths - 1)))
       (concatMap (\fp ->  mapper (snd (findFile (fsys fsstate) 
       (buildPath (normalizePath (pathToArr fp) fsstate)))))  (dropIt paths) ) ,cpath fsstate),"")
          where mapper (Just a) = a
                mapper Nothing =  ""

    rm :: FSState -> [FullPath] -> (FSState, String)
    rm fsstate paths = ((foldr (\ fp y -> removeFile y (buildPath (normalizePath (pathToArr fp) fsstate))) (fsys fsstate) paths, cpath fsstate), "")

    terminalMain :: FSState -> IO ()
    terminalMain fsstate = do
        line <- getLine
        (fst1, out) <- return (command fsstate (parseString line))
        print out
        terminalMain fst1
    
    main :: IO ()
    main = terminalMain (mkdir (touch (touch (mkdir (M.insert "" [] M.empty, M.empty) "" "d1") "/d1" "f1" "c1") "/d1" "f2" "c2") "/d1" "d2", [])
