import System.IO
import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace
import Control.Exception.Assert
import GHC.Exts (sortWith)

type Value = Maybe Int
type ValueSet = Set.Set Int
type Square = (Int,Int)
type Grid = (Int, Int)
type Puzzle = Map.Map Square Value
type Choices = Map.Map Square ValueSet
type OrderedChoices = [Square]

safeFromJust x = assert (isJust x) (fromJust x)

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
        valuesInColumn = [(\val -> assert (isJust val) (fromJust val)) $ Map.lookup (x, y) p | y <- [0..8]]
    in
        remainingInSet digitsInColumn

remainingInRow :: Puzzle -> Int -> ValueSet
remainingInRow p y =
    let
        digitsInRow = Set.fromList $ catMaybes $ valuesInRow
        valuesInRow = [(\val -> assert (isJust val) (fromJust val)) $ Map.lookup (x, y) p | x <- [0..8]]
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
    countPossibilities square = Set.size $ (\val -> assert (isJust val) (fromJust val)) $ Map.lookup square c

-- From Norvig:
-- 2 basic strategies that can be combined with search to solve even the hardest problems in less than a second.
-- 1) If a square has only one possible value, then eliminate that value from the square's peers. 
-- 2) If a unit has only one possible place for a value, then put the value there.
--
-- After trying some wrong choices, the search will start by choosing a square with 1 possible value, but after backtracking it will try out every other competing move too! this means you should implement strategy 1 explicitly.
-- Strategy 2 needs a special function to implement.

-- For every value, if a row/column/grid has exactly 1 place for that value, put the value there.
-- There could be more than one unit with a unique place, or none of them. This function returns the first one or otherwise Nothing.
findUniquePlace :: Puzzle -> Choices -> Maybe (Square, Int)
findUniquePlace p c =
    let
        uniquePlaces = catMaybes [findUniquePlaceInUnit digit unit c | digit <- [0..8], unit <- allUnits]
    in
        listToMaybe uniquePlaces

findUniquePlaceInUnit :: Int -> [Square] -> Choices -> Maybe (Square, Int)
findUniquePlaceInUnit digit squares choices =
    let
        -- squares in this unit that would allow digit as one of their choices
        -- ignore squares that have already been filled and thus aren't in the choices map
        moves = [(s, digit) | s <- squares, s `Map.member` choices, allowsDigit s]
        allowsDigit square = digit `Set.member` digitsForSquare square
        digitsForSquare square = (\val -> assert (isJust val) (fromJust val)) $ Map.lookup square choices
    in
        listToMaybe moves

allUnits :: [[Square]]
allUnits = 
    let
        allColumns = [squaresInColumn x | x <- [0..8]]
        allRows = [squaresInRow y | y <- [0..8]]
        allGrids = [squaresInGrid (gridX, gridY) | gridX <- [0..2], gridY <- [0..2]]
    in
        allColumns ++ allRows ++ allGrids

squaresInColumn x = [(x,y) | y <- [0..8]]
squaresInRow y = [(x,y) | x <- [0..8]]
squaresInGrid (gridX, gridY) =
    let
        leftX = gridX*3
        rightX = leftX + 2

        leftY = gridY*3
        rightY = leftY + 2

    in
        [ (x,y) | x <- [leftX..rightX], y <- [leftY..rightY]]    

valuesForSquare :: Choices -> Square -> [Int]
valuesForSquare c square = Set.toList $ fromJust $ Map.lookup square c

solve :: String -> Puzzle -> Maybe Puzzle
solve path p =
    let
        c = choices p
        orderedSquares = orderChoices c
        solved = 0 == Map.size c
        failed = 0 == (Set.size $ fromJust $ Map.lookup (head orderedSquares) c)

        possibleUniquePlace = findUniquePlace p c

        path' = path ++ "."

        -- try strategy 1 and strategy 2, each of which means ONLY a single move this time. Otherwise try out all of the moves in order of how many possible values per square
        movesToTry :: [Square]
        movesToTry = --trace path' $
            if 1 == (length $ valuesForSquare c (head orderedSquares)) then
                [head orderedSquares]
            else if isJust possibleUniquePlace then
                let Just (square, _) = possibleUniquePlace in
                [square]
            else
                orderedSquares

        solve' ::  Maybe Puzzle
        solve' = let
            possibleSolutions :: [Maybe Puzzle]
            possibleSolutions = [solve path' (Map.insert square (Just digit) p) | square <- movesToTry, digit <- valuesForSquare c square]

            validSolutions = catMaybes possibleSolutions
            in
                if validSolutions == [] then Nothing else Just $ head validSolutions
    in
        if solved then Just p else if failed then Nothing else solve'


main = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let maybeSolution = solve "" $ parse contents
    putStrLn $ if isJust maybeSolution then toString $ fromJust maybeSolution else "failed to find solution"


