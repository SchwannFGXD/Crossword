import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

instance NFData Orientation

type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read,Generic)


--Solves word search
solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch [] gs = []
solveWordSearch (x:xs) gs   | length gs == length (filter (== length gs) (lineSizes gs)) = checkOnCo (toUpperString x) (wordLocation (toUpperString x) (toUpperListStr gs)) (toUpperListStr gs) : solveWordSearch xs (toUpperListStr gs)
                            | otherwise = []


-- gives length of each line in a grid
lineSizes :: WordSearchGrid -> [Int]
lineSizes [] = []
lineSizes (g:gs) = length g : lineSizes gs

--searches grid for word and returns placement
checkOnCo :: String -> [(Int,Int)] -> [[Char]] -> (String, Maybe Placement)
checkOnCo x [] gs = (x,Nothing)
checkOnCo x (p:ps) gs   | checkForward x (p) gs = (x,Just ((p),Forward))
                        | checkBack x (p) gs = (x,Just ((p),Back))
                        | checkUp x (p) gs = (x,Just ((p),Up))
                        | checkDown x (p) gs = (x,Just ((p),Down))
                        | checkUpForward x (p) gs = (x,Just ((p),UpForward))
                        | checkUpBack x (p) gs = (x,Just ((p),UpBack))
                        | checkDownForward x (p) gs = (x,Just ((p),DownForward))
                        | checkDownBack x (p) gs = (x,Just ((p),DownBack))
                        | otherwise =  checkOnCo x ps gs
--provides all possible positions of a given character
gridToIndex :: Int  -> Char -> [[Char]] -> [(Int,Int)]
gridToIndex n y [] = []
gridToIndex n y (x:xs)  | elem y x = [(x,n) | x <- elemIndices y x] ++ gridToIndex (n+1) y (xs)
                        | otherwise = gridToIndex (n+1) y (xs)
--provides potential position of a given word
wordLocation :: String -> [[Char]] -> [(Int,Int)]
wordLocation y x = gridToIndex 0 (head y) x
--gives character at given position
indexToGrid :: (Int,Int) -> [[Char]] -> Char 
indexToGrid (m,n) x = (x!!n)!!m 
--checks if orientation of word is Forward
checkForward :: String  -> (Int,Int) -> [[Char]] -> Bool 
checkForward (x:xs) (m,n) gs    | (x == indexToGrid (m,n) gs) && ((x:xs) == (take (length (x:xs)) (drop (m) (gs!!n)))) = True 
                                | otherwise = False 
--checks if orientation of word is Back
checkBack :: String  -> (Int,Int) -> [[Char]] -> Bool
checkBack (x:xs) (m,n) gs   | (x == indexToGrid (m,n) gs) && ((x:xs) == reverse (drop (m +1 - (length (x:xs))) (take (m+1) (gs!!n)))) = True 
                            | otherwise = False 
--checks if orientation of word is Down                             
checkDown :: String  -> (Int,Int) -> [[Char]] -> Bool
checkDown (x:xs) (m,n) gs   | (x == indexToGrid (m,n) gs) && ((x:xs) == (take (length (x:xs)) (drop (n) ([ k | j <- gs, k <- [j!!m]])))) = True
                            | otherwise = False 
--checks if orientation of word is Up
checkUp :: String  -> (Int,Int) -> [[Char]] -> Bool
checkUp (x:xs) (m,n) gs     | (x == indexToGrid (m,n) gs) && ((x:xs) == reverse (drop (n +1 - (length (x:xs))) (take (n+1) ([ k | j <- gs, k <- [j!!m]])))) = True
                            | otherwise = False 
--checks if orientation of word is DownForward                            
checkDownForward :: String  -> (Int,Int) -> [[Char]] -> Bool
checkDownForward (x:xs) (m,n) gs| (x == indexToGrid (m,n) gs) && ((x:xs) == (take (length (x:xs)) (downForwardCon (m,n) gs))) = True 
                                | otherwise = False 
--checks if orientation of word is Upforward
checkUpForward :: String  -> (Int,Int) -> [[Char]] -> Bool
checkUpForward (x:xs) (m,n) gs  | (x == indexToGrid (m,n) gs) && ((x:xs) ==  (take (length (x:xs)) (upForwardCon (m,n) gs))) = True 
                                | otherwise = False 
--checks if orientation of word is UpBack
checkUpBack :: String  -> (Int,Int) -> [[Char]] -> Bool
checkUpBack (x:xs) (m,n) gs | (x == indexToGrid (m,n) gs) && ((x:xs) ==  (take (length (x:xs)) (upBackCon (m,n) gs))) = True 
                            | otherwise = False 
--checks if orientation of word is DownBack
checkDownBack :: String  -> (Int,Int) -> [[Char]] -> Bool
checkDownBack (x:xs) (m,n) gs   | (x == indexToGrid (m,n) gs) && ((x:xs) == (take (length (x:xs)) (downBackCon (m,n) gs))) = True 
                                | otherwise = False 
--provides elements in DownForward position from given position
downForwardCon :: (Int,Int) -> [[Char]] -> [Char]
downForwardCon (m,n) gs | n+1 > length (gs) = []
                    | m+1 > length (gs) = []
                    | otherwise = [(gs!!n)!!m] ++ downForwardCon ((m+1),(n+1)) gs
--provides elements in UpForward position from given position
upForwardCon :: (Int,Int) -> [[Char]] -> [Char]
upForwardCon (m,n) gs   | n <0 = []
                        | m+1 > length (gs) = []
                        | otherwise = [(gs!!n)!!m] ++ upForwardCon ((m+1),(n-1)) gs
--provides elements in upBack position from given position
upBackCon :: (Int,Int) -> [[Char]] -> [Char]
upBackCon (m,n) gs  | n <0 = []
                    | m <0 = []
                    | otherwise = [(gs!!n)!!m] ++ upBackCon ((m-1),(n-1)) gs
--provides elements in DownBack position from given position
downBackCon :: (Int,Int) -> [[Char]] -> [Char]
downBackCon (m,n) gs    | n+1 > length (gs) = []
                        | m <0 = []
                        | otherwise = [(gs!!n)!!m] ++ downBackCon ((m-1),(n+1)) gs

--capitalises every letter in string
toUpperString :: [Char] -> [Char]
toUpperString str = [ toUpper x | x <- str]

--capitalises every letter in list of strings
toUpperListStr :: [String ] -> [String ]
toUpperListStr str = [ toUpperString x | x <- str]









--createwordsearch
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch [] _ = pure[]
createWordSearch x y = do
                        gs <- randomPlacerGrid3 x y
                        updateGrid2 (gs) (obtainChars x)

-- gets all unique characters from a list of strings
obtainChars :: [String] -> [Char]
obtainChars [] = []
obtainChars (x:xs) = nub (nub x ++ obtainChars xs)

-- list of length of each word
wordLengths :: [String] -> [Int] 
wordLengths [] = []
wordLengths (x:xs) = length x : wordLengths xs

-- creates blank grid
blankGrid :: [ String ] -> Double -> WordSearchGrid
blankGrid x y   | x == [] = []
                | y <= 0 = error "Invalid density"
                | y >= 1 = error "Invalid density"
                | otherwise = blankGrid1 (gridSize x y) (gridSize x y) 

-- creates blank grid 
blankGrid1 :: Int -> Int -> WordSearchGrid
blankGrid1 n m  | m == 0 = []
                | otherwise = lineGrid n : blankGrid1 n (m-1)

-- creates blank lines
lineGrid :: Int -> [Char]
lineGrid 0 = []
lineGrid n = "-" ++ lineGrid (n-1)

-- calculates the grid size
gridSize :: [ String ] -> Double -> Int
gridSize [] y = error "Invalid selection of words"
gridSize (x:xs) y   | fromIntegral (maximum (wordLengths (x:xs))) > sqrt((fromIntegral (sum(wordLengths (x:xs)))) / y) = maximum (wordLengths (x:xs))
                    | otherwise = ceiling (sqrt((fromIntegral (sum(wordLengths (x:xs)))) / y))

--lowers a string of capitals into lower case
toLowerString :: [Char] -> [Char]
toLowerString str = [ toLower x | x <- str]

--sorts list of words from biggest size to smallest
rearrangeList :: [ String ] -> [ String ]
rearrangeList [] = []
rearrangeList x = reverse (rearrangeList1 x (wordLengths x) (sort (wordLengths x)))

--sorts list of words from biggest size to smallest
rearrangeList1 ::  [ String ] -> [Int] -> [Int] -> [ String ]
rearrangeList1 [] _ _ = []
rearrangeList1 (x:xs) (y:ys) (z:zs) | z == y = x : rearrangeList1 xs ys zs
                                    | otherwise = rearrangeList1 (switchList (x:xs)) (switchList (y:ys)) (z:zs)

--puts first element in a list at the end of the list
switchList :: [a] -> [a]
switchList x = tail x ++ [head x]

-- compares word with string from grid
compareBlank :: String -> String -> Bool
compareBlank [] _ = True 
compareBlank _ [] = False 
compareBlank (x:xs) (y:ys)  | length (x:xs) > length (y:ys) = False
                            | x == y = compareBlank xs ys
                            | y == '-' = compareBlank xs ys
                            | otherwise = False 

--checks if word can be placed in forward direction
canFitForward :: String  -> (Int,Int) -> WordSearchGrid -> Bool 
canFitForward x (m,n) gs    | compareBlank x (take (length x) (drop (m) (gs!!n))) = True 
                            | otherwise = False 

--checks if word can be placed in back direction
canFitBack :: String  -> (Int,Int) -> WordSearchGrid -> Bool
canFitBack x (m,n) gs   | compareBlank x (reverse (drop (m +1 - (length x)) (take (m+1) (gs!!n)))) = True 
                        | otherwise = False 

--checks if word can be placed in forward direction
canFitDown :: String  -> (Int,Int) -> WordSearchGrid -> Bool
canFitDown x (m,n) gs   | compareBlank x (take (length x) (drop (n) ([ k | j <- gs, k <- [j!!m]]))) = True
                        | otherwise = False 

--checks if word can be placed in up direction
canFitUp :: String  -> (Int,Int) -> WordSearchGrid -> Bool
canFitUp x (m,n) gs | compareBlank x (reverse (drop (n +1 - (length x)) (take (n+1) ([ k | j <- gs, k <- [j!!m]])))) = True
                    | otherwise = False 

--checks if word can be placed in downforward direction
canFitDownForward :: String  -> (Int,Int) -> WordSearchGrid -> Bool
canFitDownForward x (m,n) gs    | compareBlank x (take (length x) (downForwardCon (m,n) gs)) = True 
                                | otherwise = False

--checks if word can be placed in upforward direction
canFitUpForward :: String  -> (Int,Int) -> WordSearchGrid -> Bool
canFitUpForward x (m,n) gs  | compareBlank x (take (length x) (upForwardCon (m,n) gs)) = True 
                            | otherwise = False 

--checks if word can be placed in upback direction
canFitUpBack :: String  -> (Int,Int) -> WordSearchGrid -> Bool
canFitUpBack x (m,n) gs     | compareBlank x (take (length x) (upBackCon (m,n) gs)) = True 
                            | otherwise = False

--checks if word can be placed in downback direction
canFitDownBack :: String  -> (Int,Int) -> WordSearchGrid -> Bool
canFitDownBack x (m,n) gs   | compareBlank x (take (length x) (downBackCon (m,n) gs)) = True 
                            | otherwise = False 

--puts word in forward position on grid
replaceGridForward :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridForward x (m,n) gs = (take (n) gs) ++ [(take m (gs!!n)) ++ x ++ (drop (m + length x) (gs!!n))] ++ (drop (n+1) gs)

--puts word in back position on grid
replaceGridBack :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridBack x (m,n) gs = (take (n) gs) ++ [(take ((m+1)- length x) (gs!!n)) ++ reverse x ++ (drop (m+1) (gs!!n))] ++ (drop (n+1) gs)

--puts word in down position on grid
replaceGridDown :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridDown x (m,n) gs = (take (n) gs) ++ (replaceGridDownCon x (m,n) gs) ++ (drop (n + length x) gs)

--puts word in up position on grid
replaceGridUp :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridUp x (m,n) gs = replaceGridDown (reverse x) (m,(n + 1 - length x)) gs

--puts word in down forward position on grid
replaceGridDownForward :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridDownForward x (m,n) gs = (take (n) gs) ++ (replaceGridDownFCon x (m,n) gs) ++ (drop (n + length x) gs)

--puts word in up forward position on grid
replaceGridUpForward :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridUpForward x (m,n) gs = replaceGridDownBack (reverse x) ((m - 1 + length x),(n + 1 - length x)) gs

--puts word in up back position on grid
replaceGridUpBack :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridUpBack x (m,n) gs = replaceGridDownForward (reverse x) ((m + 1 - length x),(n + 1 - length x)) gs

--puts word in down back position on grid
replaceGridDownBack :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridDownBack x (m,n) gs = (take (n) gs) ++ replaceGridDownBCon x (m,n) gs ++ (drop (n + length x) gs)

--changes certain section of grid based on word coordinates
replaceGridDownCon :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridDownCon [] _ _ = []
replaceGridDownCon (x:xs) (m,n) gs = [(take m (gs!!n)) ++ [x] ++ (drop (m+1) (gs!!n))] ++ replaceGridDownCon xs (m,n+1) gs

--changes certain section of grid based on word coordinates
replaceGridDownFCon :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridDownFCon [] _ _ = []
replaceGridDownFCon (x:xs) (m,n) gs = [(take m (gs!!n)) ++ [x] ++ (drop (m+1) (gs!!n))] ++ replaceGridDownFCon xs (m+1,n+1) gs

--changes certain section of grid based on word coordinates
replaceGridDownBCon :: String  -> (Int,Int) -> WordSearchGrid -> WordSearchGrid
replaceGridDownBCon [] _ _ = []
replaceGridDownBCon (x:xs) (m,n) gs = [(take m (gs!!n)) ++ [x] ++ (drop (m+1) (gs!!n))] ++ replaceGridDownBCon xs (m-1,n+1) gs

--provides first value 
divider1 :: (WordSearchGrid, [String], [String]) -> WordSearchGrid
divider1 (x,y,z) = x

--provides second value
divider2 :: (WordSearchGrid, [String], [String]) -> [String]
divider2 (x,y,z) = y

--provides third value
divider3 :: (WordSearchGrid, [String], [String]) -> [String]
divider3 (x,y,z) = z

--randomy places a word in a grid in a random position
randomPlacerGrid ::  [String] -> [String] -> Double -> IO WordSearchGrid -> Int -> IO (WordSearchGrid, [String], [String] )
randomPlacerGrid (x:xs) (c:cs) y gs z = do 
                            gs <- gs
                            m <- randomRIO (0,(length (gs))-1)
                            n <- randomRIO (0,(length (gs))-1)
                            a <- randomRIO ('a','h')

                            if z == 10000 then return (gs, rearrangeList (x:xs++cs), [c])
                            else if (a == 'a') && canFitForward x (m,n) gs then return ((replaceGridForward x (m,n) gs), xs, c:cs++[x])
                            else if (a == 'b') && canFitBack x (m,n) gs then return ((replaceGridBack x (m,n) gs), xs, c:cs++[x])
                            else if (a == 'c') && canFitUp x (m,n) gs then return ((replaceGridUp x (m,n) gs), xs, c:cs++[x])
                            else if (a == 'd') && canFitDown x (m,n) gs then return ((replaceGridDown x (m,n) gs), xs, c:cs++[x])
                            else if (a == 'e') && canFitUpForward x (m,n) gs then return ((replaceGridUpForward x (m,n) gs), xs, c:cs++[x])
                            else if (a == 'f') && canFitUpBack x (m,n) gs then return ((replaceGridUpBack x (m,n) gs), xs, c:cs++[x])
                            else if (a == 'g') && canFitDownForward x (m,n) gs then return ((replaceGridDownForward x (m,n) gs), xs, c:cs++[x])
                            else if (a == 'h') && canFitDownBack x (m,n) gs then return ((replaceGridDownBack x (m,n) gs), xs, c:cs++[x])
                            else randomPlacerGrid (x:xs) (c:cs) y (pure gs) (z+1)

--randomly places a word in a grid
randomPlacerGrid1 :: [String] -> [String] -> Double -> WordSearchGrid -> IO (WordSearchGrid, [String], [String] )
randomPlacerGrid1 (x:xs) (c:cs) y gs = randomPlacerGrid (x:xs) (c:cs) y (pure gs) 0

-- recursively places words in a grid
randomPlacerGrid2 :: Double -> IO (WordSearchGrid, [String], [String] ) -> IO WordSearchGrid
randomPlacerGrid2 y gs = do 
                        hs <- gs
                        if (divider2 hs) == [] then return (divider1 hs)
                        else do
                        hs <-gs
                        is <- randomPlacerGrid1 (divider2 hs) (divider3 hs) y (divider1 hs)
                        randomPlacerGrid2 y (pure is)

-- recursively places words in a grid
randomPlacerGrid3 :: [String] -> Double -> IO WordSearchGrid
randomPlacerGrid3 (x:xs) y = randomPlacerGrid2 (y) (pure((blankGrid (x:xs) y),rearrangeList (x:xs),["XD"]))

--generates a random character
randomCharGen :: [Char] -> IO Char 
randomCharGen x = do
                y <- randomRIO ('a','z')
                if elem y (toLowerString x)
                    then return(toUpper y)
                    else do
                        randomCharGen x


--replaces character at given coordinate with random character
replaceChar3 :: IO WordSearchGrid -> (Int,Int) -> [Char] -> IO WordSearchGrid
replaceChar3 gs (m,n) y = do 
                        a <- (randomCharGen y) 
                        gs <- gs
                        return (updateGrid gs (m,n) a)

-- replaces character at given coordinate 
updateGrid :: WordSearchGrid -> (Int,Int) -> Char -> WordSearchGrid
updateGrid gs (m,n) y = (take (n) gs) ++ [(take m (gs!!n)) ++ [y] ++ (drop (m+1) (gs!!n))] ++ (drop (n+1) gs)

-- replaces all characters in grid
updateGrid1 :: IO WordSearchGrid -> (Int,Int) -> [Char] -> Int -> IO WordSearchGrid
updateGrid1 gs (m,n) y x    | m == x && n == x - 1 = gs
                            | m == x = updateGrid1 gs (0,n+1) y x
                            | otherwise = do
                                grid <- gs
                                a <- (randomCharGen y)
                                if indexToGrid (m,n) grid == '-' then updateGrid1 (pure(updateGrid grid (m,n) (a))) (m+1,n) y x
                                else updateGrid1 (pure(updateGrid grid (m,n) (toUpper (indexToGrid (m,n) grid)))) (m+1,n) y x
    
--replaces all characters in grid
updateGrid2 :: WordSearchGrid -> [Char] -> IO WordSearchGrid
updateGrid2 gs y = updateGrid1 (pure gs) (0,0) y (length gs)


createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws


--example grids
exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]

