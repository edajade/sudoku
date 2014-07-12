import System.IO
import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace
import GHC.Exts (sortWith)

type Value = Maybe Int
type ValueSet = Set.Set Int
type Square = (Int,Int)
type Grid = (Int, Int)
type Puzzle = Map.Map Square Value
type Choices = Map.Map Square ValueSet
type OrderedChoices = [Square]

emptyPuzzle :: Puzzle
emptyPuzzle = Map.empty

-- parse a puzzle
parse :: String -> Puzzle
parse file = do
    let rows = (lines file) :: [String]
    processRows rows 0 emptyPuzzle

processRows :: [String] -> Int -> Puzzle -> Puzzle
processRows [] y p = p
processRows ("-----------":rest_rows) y p = processRows rest_rows y p
processRows (row:rest_rows) y p = do
    let p' = processColumns row 0 y p
    processRows rest_rows (y+1) p'
    
processColumns :: String -> Int -> Int -> Puzzle -> Puzzle
processColumns "" x y p = p
processColumns ('|':rest_chars) x y p = processColumns rest_chars x y p
processColumns (char:rest_chars) x y p = do
    let value = (if char=='.' then Nothing else Just (read [char])) :: Value
    let p' = Map.insert (x, y) value p
    processColumns rest_chars (x+1) y p'

-- display a puzzle
toString :: Puzzle -> String
toString p =
    showRows (stringValues p) where
    showRows "" = ""
    showRows values = take 9 values ++ "\n" ++ showRows (drop 9 values)

stringValues :: Puzzle -> String
stringValues p = 
    foldr displaySquare "" (Map.toList p)
    where
        displaySquare (square, Nothing) str = str++ "."
        displaySquare (square, Just digit) str = str++ show digit

--
gridOf :: Square -> Grid
gridOf (x,y) = (x `quot` 3, y `quot` 3)

remainingInSet :: ValueSet -> ValueSet
remainingInSet s = Set.difference allNumbers s where
    allNumbers = Set.fromList [1..9]

remainingInColumn :: Puzzle -> Int -> ValueSet
remainingInColumn p x =
    let
        digitsInColumn = Set.fromList $ catMaybes $ valuesInColumn
        valuesInColumn = [fromJust $ Map.lookup (x, y) p | y <- [0..8]]
    in
        remainingInSet digitsInColumn

remainingInRow :: Puzzle -> Int -> ValueSet
remainingInRow p y =
    let
        digitsInRow = Set.fromList $ catMaybes $ valuesInRow
        valuesInRow = [fromJust $ Map.lookup (x, y) p | x <- [0..8]]
    in
        remainingInSet digitsInRow

remainingInGrid :: Puzzle -> Grid -> ValueSet
remainingInGrid p (x,y) =
    let
        (gridX, gridY) = gridOf (x,y)

        leftX = gridX*3
        rightX = leftX + 2

        leftY = gridY*3
        rightY = leftY + 2

        valuesInGrid = [fromJust $ Map.lookup (x, y) p | x <- [leftX..rightX], y <- [leftY..rightY]]
        digitsInGrid = Set.fromList $ catMaybes $ valuesInGrid
    in
        remainingInSet digitsInGrid

choicesForSquare :: Puzzle -> Square -> ValueSet
choicesForSquare p (x,y) =
    let
        row = remainingInRow p y
        column = remainingInColumn p x
        grid = remainingInGrid p (x, y)
    in
        row `Set.intersection`  column `Set.intersection` grid

choices :: Puzzle -> Choices
choices p =
    Map.fromList [ (square, choicesForSquare p square) | (square, value) <- Map.toList p, isNothing value]

orderChoices :: Choices -> OrderedChoices
orderChoices c =
    sortWith countPossibilities (Map.keys c) where
    countPossibilities square = Set.size $ fromJust $ Map.lookup square c

solve :: Puzzle -> Maybe Puzzle
solve p =
    let
        c = choices p
        orderedSquares = orderChoices c
        solved = 0 == Map.size c
        failed = 0 == (Set.size $ fromJust $ Map.lookup (head orderedSquares) c)

        solve' ::  Maybe Puzzle
        solve' = let
            possibleSolutions :: [Maybe Puzzle]
            possibleSolutions = [solve (Map.insert square (Just digit) p) | square <- orderedSquares, digit <- Set.toList $ fromJust $ Map.lookup square c]

            validSolutions = catMaybes possibleSolutions
            in
                if validSolutions == [] then Nothing else Just $ head validSolutions
    in
        if solved then Just p else if failed then Nothing else solve'


main = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    putStrLn $ toString $ fromJust $ solve $ parse contents

