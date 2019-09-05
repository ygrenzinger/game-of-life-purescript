module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (CellState(..), GameOfLife(..), countAliveNeighbours, createEmptyGrid, createFullGrid, fromAsciiArt, makeAliveAt, nextCellState, nextGeneration)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "game of life" do
    -- generate list of numbers or using PBT would increase test quality here
    describe "Cell State rules" do
      it "Any live cell with fewer than two live neighbours dies, as if by underpopulation." do
        (nextCellState 0 Alive)  `shouldEqual` Dead
        (nextCellState 1 Alive)  `shouldEqual` Dead
      it "Any live cell with two or three live neighbours lives on to the next generation." do
        (nextCellState 2 Alive)  `shouldEqual` Alive
        (nextCellState 3 Alive)  `shouldEqual` Alive
      it "Any live cell with more than three live neighbours dies, as if by overpopulation." do
        (nextCellState 4 Alive)  `shouldEqual` Dead
        (nextCellState 8 Alive)  `shouldEqual` Dead
      it "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction." do
        (nextCellState 3 Dead)  `shouldEqual` Alive
      it "Any dead cell without exactly three live neighbours stays dead." do
        (nextCellState 2 Dead)  `shouldEqual` Dead
        (nextCellState 8 Dead)  `shouldEqual` Dead
    describe "Computing alive neighbours" do
      it "no alive neighbours" do
        let emptyGrid = (createEmptyGrid 3)
        let grid = fromMaybe emptyGrid (makeAliveAt 1 1 emptyGrid)
        (countAliveNeighbours 1 1 grid) `shouldEqual` 0
      it "all alive neighbours" do
        let grid = (createFullGrid 3)
        (countAliveNeighbours 1 1 grid) `shouldEqual` 8
    describe "from Ascii Art" do
      it "should convert string to GameOfLife" do
        let gameOfLife = fromAsciiArt "xxx\n\
                                      \xxx\n\
                                      \xxx"
        gameOfLife `shouldEqual` (Just (GameOfLife 3 (createFullGrid 3)))               
    describe "Validating generation with patterns" do
      it "should handle Still lifes pattern with block example" do
        let gameOfLife = fromAsciiArt "oooo\n\
                                      \oxxo\n\
                                      \oxxo\n\
                                      \oooo"
        let nextGameOfLife = gameOfLife >>= \g ->  nextGeneration g
        nextGameOfLife `shouldEqual` nextGameOfLife
      it "should handle Oscillators pattern with blinker example" do
        let gameOfLife = fromAsciiArt "ooooo\n\
                                      \ooxoo\n\
                                      \ooxoo\n\
                                      \ooxoo\n\
                                      \ooooo"
        let expected = fromAsciiArt "ooooo\n\
                                    \ooooo\n\
                                    \oxxxo\n\
                                    \ooooo\n\
                                    \ooooo"
        let nextGameOfLife = gameOfLife >>= \g ->  nextGeneration g
        nextGameOfLife `shouldEqual` expected
