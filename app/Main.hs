module Main (main) where

import qualified Data.Set as Set
import Control.Monad (forM_, unless)
import System.Random (newStdGen, randomRs)
import Text.Read (readMaybe)

type Cell = (Int, Int)
type Board = Set.Set Cell


defaultBoardDimension :: Int
defaultBoardDimension = 20


patternOptions :: [(String, Board)]
patternOptions = [("glider", gliderPattern), ("random", Set.empty), ("lwss", lwssPattern), ("pulsar", pulsarPattern)]


gliderPattern :: Board
gliderPattern = Set.fromList [(2, 0), (2, 1), (2, 2), (1, 2), (0, 1)]


lwssPattern :: Board
lwssPattern = Set.fromList [(1, 0), (4, 0), (0, 1), (0, 2), (4, 2), (0, 3), (1, 3), (2, 3), (3, 3)]


pulsarPattern :: Board
pulsarPattern = Set.fromList 
    [(2, 0), (3, 0), (4, 0), (8, 0), (9, 0), (10, 0),
     (0, 2), (5, 2), (7, 2), (12, 2), (0, 3), (5, 3), (7, 3), (12, 3), (0, 4), (5, 4), (7, 4), (12, 4),
     (2, 5), (3, 5), (4, 5), (8, 5), (9, 5), (10, 5),
     (2, 7), (3, 7), (4, 7), (8, 7), (9, 7), (10, 7),
     (0, 8), (5, 8), (7, 8), (12, 8), (0, 9), (5, 9), (7, 9), (12, 9), (0, 10), (5, 10), (7, 10), (12, 10),
     (2, 12), (3, 12), (4, 12), (8, 12), (9, 12), (10, 12)]


randomPattern :: Int -> Int -> IO Board
randomPattern width height = do
  gen <- newStdGen
  let randomValues = take (width * height) $ randomRs (False, True) gen
  return $ Set.fromList [ (x, y) | x <- [0..width-1], y <- [0..height-1], randomValues !! (y * width + x) ]


initializeBoard :: String -> Int -> IO Board
initializeBoard patternType size =
  case patternType of
    "random" -> randomPattern size size
    _        -> case lookup patternType patternOptions of
                  Just pattern -> return pattern
                  Nothing      -> return Set.empty


gameLoop :: Board -> Int -> Int -> IO ()
gameLoop board size generation = do
  printBoard board size generation
  putStrLn "'Enter': continue, 'q': quit:"
  command <- getLine
  case command of
    ""  -> gameLoop (nextGeneration board) size (generation + 1)
    "q" -> return ()
    _   -> gameLoop board size generation


nextGeneration :: Board -> Board
nextGeneration board = Set.filter shouldBeAlive allCellsToConsider
  where
    allCellsToConsider = Set.unions [neighbours cell | cell <- Set.toList board] `Set.union` board
    shouldBeAlive cell = let aliveNeighbours = Set.size (Set.intersection (neighbours cell) board)
                         in aliveNeighbours == 3 || aliveNeighbours == 2 && Set.member cell board


neighbours :: Cell -> Set.Set Cell
neighbours (x, y) = Set.fromList [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], not (dx == 0 && dy == 0)]


printBoard :: Board -> Int -> Int -> IO ()
printBoard board size generation = do
    putStr "\ESC[2J"  -- Clear screen
    putStrLn $ "Generation: " ++ show generation
    forM_ [0..size-1] $ \y -> do
      forM_ [0..size-1] $ \x -> do
        putStr $ if Set.member (x, y) board then "█" else " "
      putStrLn ""


chooseOption :: String -> [String] -> IO String
chooseOption prompt options = do
    putStrLn prompt
    mapM_ putStrLn $ zipWith (\i option -> show i ++ ": " ++ option) [1..] options
    input <- getLine
    case readMaybe input of
        Just n | n > 0 && n <= length options -> return (options !! (n - 1))
        _ -> do
            putStrLn "Invalid input, please try again."
            chooseOption prompt options


chooseInt :: String -> Int -> IO Int
chooseInt prompt defaultValue = do
    putStrLn prompt
    input <- getLine
    return $ maybe defaultValue id (readMaybe input :: Maybe Int)


main :: IO ()
main = do
    patternType <- chooseOption "Choose a pattern" (map fst patternOptions)
    size <- chooseInt "Choose a board size (int)" defaultBoardDimension
    board <- initializeBoard patternType size
    gameLoop board size 0
