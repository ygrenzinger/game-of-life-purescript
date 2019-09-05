module Main where

import Data.Array ((!!), concat, filter, foldM, length, replicate, updateAt, range)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

data CellState = Alive | Dead
derive instance eqCellState :: Eq CellState
instance showCellState :: Show CellState where
  show Dead = "o"
  show Alive = "x"

--newtype Position = Position (Tuple Int Int)

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

isAlive :: Int -> Int -> Grid -> Boolean
isAlive rowIndex columnIndex grid = 
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
makeAliveAt = (updateCellStateAt Alive)

createGrid :: CellState -> Int -> Grid
createGrid cellState size = replicate size (replicate size cellState) 

createEmptyGrid :: Int -> Grid
createEmptyGrid = (createGrid Dead)

createFullGrid :: Int -> Grid
createFullGrid = (createGrid Alive)

countAliveNeighbours :: Int -> Int -> Grid -> Int
countAliveNeighbours rowIndex columnIndex grid = 
  let 
    rowRange = range (rowIndex - 1) (rowIndex + 1)
    columnRange = range (columnIndex - 1) (columnIndex + 1)
    positions = concat $ map (\i -> map (\j -> (Tuple i j)) columnRange) rowRange
    aliveFilter = \ (Tuple i j) -> not (i == rowIndex && j == columnIndex) && (isAlive i j grid)
    alivePositions = filter aliveFilter positions
  in
    length alivePositions

nextGrid :: Grid -> Grid -> Tuple Int Int -> Maybe Grid
nextGrid oldGrid grid (Tuple i j) = do
  let count = countAliveNeighbours i j oldGrid
  cellState <- cellAt i j oldGrid
  updateCellStateAt (nextCellState count cellState) i j grid

nextGeneration :: GameOfLife -> Maybe GameOfLife
nextGeneration (GameOfLife size previousGrid) = do
  let emptyGrid = (createEmptyGrid size)
  let indexRange = range 0 (size-1)
  let positions = concat $ map (\i -> map (\j -> (Tuple i j)) indexRange) indexRange
  newGrid <- foldM (nextGrid previousGrid) emptyGrid positions
  pure (GameOfLife size newGrid)

fromAsciiArt :: String -> Maybe GameOfLife
fromAsciiArt _ = Nothing

main :: Effect Unit
main = do
  log "Hello sailor!"
