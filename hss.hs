import Data.List

data Cell = Blank | Known Int | Potential [Int] deriving (Show, Eq)

printCellKnown :: Cell -> String
printCellKnown (Known i) = show i
printCellKnown _ = " "

printCellPotential :: Cell -> String
printCellPotential Blank = "?"
printCellPotential (Known i) = "    " ++ show i ++ "!   "
printCellPotential (Potential is) = potential ++ concat (replicate (9 - length potential) " ")
  where
    potential = foldl1 (++) $ map show is

newtype Row = Row {getRow :: [Cell]} deriving Show
newtype Column = Column {getColumn :: [Cell]} deriving Show
newtype Square = Square {getSquare :: [Cell]} deriving Show

newtype Puzzle = Puzzle {getPuzzle :: [Cell]} deriving Show

class Arrayable a where
  get :: a -> [Cell]

instance Arrayable Row where get = getRow
instance Arrayable Column where get = getColumn
instance Arrayable Square where get = getSquare

class PrettyPrintable a where
  printKnown :: a -> String
  printPotential :: a -> String

instance PrettyPrintable Row where
  printKnown row = unwords $ map printCellKnown $ getRow row
  printPotential row = unwords $ map printCellPotential $ getRow row

instance PrettyPrintable Column where
  printKnown column = intercalate "\n" $ map printCellKnown $ getColumn column
  printPotential column = intercalate "\n" $ map printCellPotential $ getColumn column

by :: Int -> [a] -> [[a]]
by _ [] = []
by n xs = take n xs : by n (drop n xs)

printSquare :: (Row -> String) -> Square -> String
printSquare printFunction square = intercalate "\n" $ map (printFunction . Row) $ by 3 $ getSquare square

instance PrettyPrintable Square where
  printKnown = printSquare printKnown
  printPotential = printSquare printPotential

printPuzzle :: (Row -> String) -> Puzzle -> String
printPuzzle printFunction puzzle = intercalate "\n" $ map (printFunction . Row) $ by 9 $ getPuzzle puzzle

instance PrettyPrintable Puzzle where
  printKnown = printPuzzle printKnown
  printPotential = printPuzzle printPotential

emptyPuzzle :: Puzzle
emptyPuzzle = Puzzle $ replicate 81 Blank

toRows :: Puzzle -> [Row]
toRows = map Row . by 9 . getPuzzle

nthRow :: Int -> Puzzle -> Row
nthRow n puzzle = toRows puzzle !! n

toColumns :: Puzzle -> [Column]
toColumns = map Column . transpose . by 9 . getPuzzle

nthColumn :: Int -> Puzzle -> Column
nthColumn n puzzle = toColumns puzzle !! n

everyNth :: Int -> Int -> [a] -> [a]
everyNth start n = concatMap (take 1) . by n . drop start

groupByIndex :: Int -> [a] -> [[a]]
groupByIndex n xs = map everyNthByIndex [0..(n-1)]
  where
    everyNthByIndex index = everyNth index n xs

toSquares :: Puzzle -> [[Square]]
toSquares = map (map (Square . concat)) . groupEveryThird . by 9 . by 3 . getPuzzle
  where
    groupEveryThird = map (groupByIndex 3)

nmthSquare :: Int -> Int -> Puzzle -> Square
nmthSquare squareColNr squareRowNr puzzle = toSquares puzzle !! squareRowNr !! squareColNr

findSquare :: Int -> Int -> Int
findSquare colNr rowNr = undefined

type Clue = (Cell, Int, Int)
type ClueIndex = (Cell, Int)

insertClueByIndex :: ClueIndex -> Puzzle -> Puzzle
insertClueByIndex (value, index) inputPuzzle =
  Puzzle (take index puzzle ++ [value] ++ drop (index + 1) puzzle)
    where
      puzzle = getPuzzle inputPuzzle

insertClue :: Clue -> Puzzle -> Puzzle
insertClue (value, colNr, rowNr) inputPuzzle
  | colNr > 8 = error $ "Column number out of bounds: " ++ show colNr
  | rowNr > 8 = error $ "Row number out of bounds: " ++ show rowNr
  | otherwise = insertClueByIndex (value, index) inputPuzzle
    where
      index = rowNr * 9 + colNr

fillColumn :: Int -> Column -> Puzzle -> Puzzle
fillColumn colNr col inputPuzzle = foldl insertSingle inputPuzzle (zip (getColumn col) [0..])
  where
    insertSingle puzzle (val, index) = insertClue (val, colNr, index) puzzle

fillRow :: Int -> Row -> Puzzle -> Puzzle
fillRow rowNr row inputPuzzle = foldl insertSingle inputPuzzle (zip (getRow row) [0..])
  where
    insertSingle puzzle (val, index) = insertClue (val, index , rowNr) puzzle

fillSquare :: Int -> Square -> Puzzle -> Puzzle
fillSquare = undefined

getInfoFromUnit :: Arrayable a => a -> [Int]
getInfoFromUnit = foldl extractKnown [] . get
  where
    extractKnown knowns (Known value) = knowns ++ [value]
    extractKnown knowns _ = knowns

getPotentialForCell :: Puzzle -> Int -> Int -> [Int]
getPotentialForCell puzzle colNr rowNr = [1..9] \\ nub (inRow ++ inColumn ++ inSquare)
  where
    inRow = getInfoFromUnit $ nthRow rowNr puzzle
    inColumn = getInfoFromUnit $ nthColumn colNr puzzle
    inSquare = getInfoFromUnit $ nmthSquare (div colNr 3) (div rowNr 3) puzzle

getPotentialForIndexCell :: Puzzle -> Int -> [Int]
getPotentialForIndexCell inputPuzzle index = getPotentialForCell inputPuzzle colNr rowNr
  where
    rowNr = div index 9
    colNr = mod index 9

fillWithPotential :: Puzzle -> Puzzle
fillWithPotential inputPuzzle = Puzzle $ zipWith mapCell (getPuzzle inputPuzzle) [0..]
  where
    mapCell :: Cell -> Int -> Cell
    mapCell (Known value) _ = Known value
    mapCell (Potential potential) _ = Potential potential
    mapCell Blank index = Potential $ getPotentialForIndexCell inputPuzzle index

isSinglePotential :: Cell -> Bool
isSinglePotential (Potential potentials) | length potentials == 1 = True
                                         | otherwise = False
isSinglePotential _ = False

firstSingle :: Puzzle -> Cell
firstSingle = head . filter isSinglePotential . getPuzzle

firstSingleIndex :: Cell -> Puzzle -> Maybe Int
firstSingleIndex firstSingle inputPuzzle = elemIndex firstSingle $ getPuzzle inputPuzzle

singlePotentialToKnown :: Cell -> Cell
singlePotentialToKnown (Potential potentials) = Known (head potentials)
singlePotentialToKnown any = any

potentialToBlank :: Cell -> Cell
potentialToBlank (Potential _) = Blank
potentialToBlank any = any

insertByMaybeIndex :: Cell -> Maybe Int -> Puzzle -> Puzzle
insertByMaybeIndex _ Nothing = id
insertByMaybeIndex cell (Just index) = insertClueByIndex (cell, index)

firstSinglePotential :: Puzzle -> Puzzle
firstSinglePotential inputPuzzle = insertByMaybeIndex
  (singlePotentialToKnown (firstSingle inputPuzzle))
  (firstSingleIndex (firstSingle inputPuzzle) inputPuzzle)
  inputPuzzle

potentialsToBlank :: Puzzle -> Puzzle
potentialsToBlank = Puzzle . map potentialToBlank . getPuzzle

step :: Puzzle -> Puzzle
step = fillWithPotential . potentialsToBlank . firstSinglePotential

solve :: Puzzle -> IO ()
solve = printList 80 . iterate step . fillWithPotential

parseStringPuzzle :: [String] -> Puzzle
parseStringPuzzle inputString = foldl foldRows emptyPuzzle (zip inputString [0..])
  where
    parseChar :: Char -> Cell
    parseChar ' ' = Blank
    parseChar c = Known (read [c] :: Int)

    foldRows :: Puzzle -> (String, Int) -> Puzzle
    foldRows puzzle (str, i) = fillRow i (Row $ map parseChar str) puzzle

wikipediaPuzzleStrings = [
  "53  7    ",
  "6  195   ",
  " 98    6 ",
  "8   6   3",
  "4  8 3  1",
  "7   2   6",
  " 6    28 ",
  "   419  5",
  "    8  79"]

evilPuzzleStrings = [
  " 6 4   9 ",
  "  1  5   ",
  "     1  4",
  "61 2 7  3",
  "4       1",
  "3  1 6 52",
  "5  9     ",
  "   7  8  ",
  " 8   3 7 "]

hardPuzzleStrings = [
  "1 5   863",
  "  3  67  ",
  "  9 8  1 ",
  " 148    2",
  " 9     7 ",
  "5    738 ",
  " 3  5 4  ",
  "  67  1  ",
  "851   9 7"]

evilPuzzle = parseStringPuzzle evilPuzzleStrings
hardPuzzle = parseStringPuzzle hardPuzzleStrings

wikipediaPuzzle = parseStringPuzzle wikipediaPuzzleStrings
wikiPotential = fillWithPotential wikipediaPuzzle

pp :: PrettyPrintable a => a -> IO ()
pp = putStrLn . printPotential

printList :: PrettyPrintable a => Int -> [a] -> IO ()
printList number = sequence_ . intersperse (putStrLn "") . map pp . take number

main = putStrLn $ printPotential $ fillWithPotential wikipediaPuzzle
