module GameOfLife where
  
import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, foldM, length, replicate, updateAt, (!!), (..))
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

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

gridFrom :: GameOfLife -> Grid
gridFrom (GameOfLife _ grid) = grid

nextCellState :: Int -> CellState -> CellState
nextCellState 3 _ = Alive
nextCellState 2 Alive = Alive
nextCellState _ _ = Dead

cellAt :: GameOfLife -> Position -> Maybe CellState
cellAt (GameOfLife _ grid) (Tuple rowIndex columnIndex) = (grid !! rowIndex) >>= \row -> row !! columnIndex

isAlive :: GameOfLife -> Position -> Boolean
isAlive gameOfLife position = 
    case (cellAt gameOfLife position) of
      Nothing -> false
      Just Dead -> false
      Just Alive -> true

updateCellStateAt :: CellState -> GameOfLife -> Position -> Maybe GameOfLife
updateCellStateAt cellState (GameOfLife size grid) (Tuple rowIndex columnIndex) = do
  row         <- grid !! rowIndex
  updatedRow  <- updateAt columnIndex cellState row
  newGrid <- updateAt rowIndex updatedRow grid
  pure (GameOfLife size newGrid)

makeAliveAt :: GameOfLife -> Position -> Maybe GameOfLife
makeAliveAt = updateCellStateAt Alive

createGrid :: CellState -> Int -> GameOfLife
createGrid cellState size = GameOfLife size $ replicate size (replicate size cellState) 

createEmptyGrid :: Int -> GameOfLife
createEmptyGrid = createGrid Dead

createFullGrid :: Int -> GameOfLife
createFullGrid = createGrid Alive

buildNeighboringPosition :: Position -> Array Position
buildNeighboringPosition (Tuple rowIndex columnIndex) = do 
  i <- (rowIndex - 1)..(rowIndex + 1)
  j <- (columnIndex - 1)..(columnIndex + 1)
  guard $ not (i == rowIndex && j == columnIndex)
  pure $ Tuple i j

countAliveNeighbours :: GameOfLife -> Position -> Int
countAliveNeighbours gameOfLife position = 
  length $ filter (isAlive gameOfLife) (buildNeighboringPosition position)

nextGenerationForPosition :: GameOfLife -> GameOfLife -> Position -> Maybe GameOfLife
nextGenerationForPosition currentGen nextGen position = do
  let count = countAliveNeighbours currentGen position
  cellState <- cellAt currentGen position
  updateCellStateAt (nextCellState count cellState) nextGen position

buildGridPositions :: Int -> Array Position
buildGridPositions gridSize = do
  i <- 0..(gridSize-1)
  j <- 0..(gridSize-1)
  pure $ Tuple i j

nextGeneration :: GameOfLife -> Maybe GameOfLife
nextGeneration gen@(GameOfLife size _) = do
  let emptyGrid = (createEmptyGrid size)
  foldM (nextGenerationForPosition gen) emptyGrid (buildGridPositions size)

fromAsciiArt :: String -> Maybe GameOfLife
fromAsciiArt input = do
    let rows = split (Pattern "\n") input
    grid <- sequence $ map (\row -> sequence $ map read (toCharArray row)) rows
    pure $ GameOfLife (length grid) grid