module GameOfLife where
  
import Prelude

import Data.Array (filter, foldM, length, replicate, updateAt, (!!), (..))
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Control.MonadZero (guard)

data CellState = Alive | Dead
derive instance eqCellState :: Eq CellState
instance showCellState :: Show CellState where
  show Dead = "_"
  show Alive = "x"

read :: Char -> Maybe CellState
read 'x' = Just Alive
read '_' = Just Dead
read _ = Nothing

type Position = Tuple Int Int

type Grid = Array (Array CellState)

data GameOfLife = GameOfLife Int Grid
derive instance eqGameOfLife :: Eq GameOfLife

instance showGameOfLife :: Show GameOfLife 
  where show (GameOfLife _ grid) = "Game of life " <> (show grid)

nextCellState :: Int -> CellState -> CellState
nextCellState 3 _ = Alive
nextCellState 2 Alive = Alive
nextCellState _ _ = Dead

cellAt :: Int -> Int -> Grid -> Maybe CellState
cellAt rowIndex columnIndex grid = (grid !! rowIndex) >>= \row -> row !! columnIndex

isAlive :: Grid -> Position -> Boolean
isAlive grid (Tuple rowIndex columnIndex) = 
    case (cellAt rowIndex columnIndex grid) of
      Nothing -> false
      Just Dead -> false
      Just Alive -> true

updateCellStateAt :: CellState -> Int -> Int -> Grid -> Maybe Grid
updateCellStateAt cellState rowIndex columnIndex grid = do
  row         <- grid !! rowIndex
  updatedRow  <- updateAt columnIndex cellState row
  updateAt rowIndex updatedRow grid

makeAliveAt :: Int -> Int -> Grid -> Maybe Grid
makeAliveAt = updateCellStateAt Alive

createGrid :: CellState -> Int -> Grid
createGrid cellState size = replicate size (replicate size cellState) 

createEmptyGrid :: Int -> Grid
createEmptyGrid = createGrid Dead

createFullGrid :: Int -> Grid
createFullGrid = createGrid Alive

buildNeighboringPosition :: Int -> Int -> Array Position
buildNeighboringPosition rowIndex columnIndex = do 
  i <- (rowIndex - 1)..(rowIndex + 1)
  j <- (columnIndex - 1)..(columnIndex + 1)
  guard $ not (i == rowIndex && j == columnIndex)
  pure $ Tuple i j

countAliveNeighbours :: Int -> Int -> Grid -> Int
countAliveNeighbours rowIndex columnIndex grid = 
  length $ filter (isAlive grid) (buildNeighboringPosition rowIndex columnIndex)

nextGrid :: Grid -> Grid -> Tuple Int Int -> Maybe Grid
nextGrid oldGrid grid (Tuple i j) = do
  let count = countAliveNeighbours i j oldGrid
  cellState <- cellAt i j oldGrid
  updateCellStateAt (nextCellState count cellState) i j grid

buildGridPositions :: Int -> Array Position
buildGridPositions gridSize = do
  i <- 0..(gridSize-1)
  j <- 0..(gridSize-1)
  pure $ Tuple i j

nextGeneration :: GameOfLife -> Maybe GameOfLife
nextGeneration (GameOfLife size previousGrid) = do
  let emptyGrid = (createEmptyGrid size)
  newGrid <- foldM (nextGrid previousGrid) emptyGrid (buildGridPositions size)
  pure $ GameOfLife size newGrid

fromAsciiArt :: String -> Maybe GameOfLife
fromAsciiArt input = do
    let rows = split (Pattern "\n") input
    grid <- sequence $ map (\row -> sequence $ map read (toCharArray row)) rows
    pure $ GameOfLife (length grid) grid