module Components.GameOfLife where
  
import Prelude

import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect(..))
import Effect.Console (log)
import Effect.Timer (IntervalId, setInterval)
import GameOfLife (CellState(..), Position, createEmptyGrid, gridFrom, makeAliveAt)
import React.Basic (Component, JSX, Self(..), StateUpdate(..), createComponent, empty, make, runUpdate)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

component :: Component Props
component = createComponent "Game of Life"

type Props = { 
    size :: Int
}
      
data Action = NewGrid
    | StartTicking
    | StopTicking
    | Tick
    | Activating Position

gameOfLife :: Props -> JSX
gameOfLife = make component { initialState, render }
  where
    initialState = { gameOfLife: Nothing, intervalId: Nothing  }

    update self = case _ of
        NewGrid -> 
          UpdateAndSideEffects
            (self.state { gameOfLife = Just $ createEmptyGrid self.props.size })
            \_ -> log $ "Create new game of life"
        StartTicking -> 
          SideEffects \s -> do 
            void $ setInterval 1000 do
              send s Tick 
        StopTicking -> NoUpdate
        Tick -> NoUpdate
        Activating position -> 
          UpdateAndSideEffects
            (self.state { gameOfLife = self.state.gameOfLife >>= \g -> makeAliveAt g position })
            \_ -> log $ "Making alive cell at : " <> show position

    send = runUpdate update

    displayCell self rowIndex columnIndex cellState = R.span
        { onClick: capture_ $ send self (Activating (Tuple rowIndex columnIndex))
        , className: className
        }
      where 
        className = "cell " <> (if (cellState == Alive) then "alive" else "dead")

    displayRow self rowIndex row = R.div {
      className: "row"
    , children: mapWithIndex (displayCell self rowIndex) row
    }

    displayGrid self grid = R.div {
      children: mapWithIndex (displayRow self) grid
    }

    displayGameOfLife self = case (self.state.gameOfLife) of
      Nothing -> empty
      Just g -> displayGrid self (gridFrom g)

    render self =
        R.div_ [
              R.button
              { onClick: capture_ $ send self NewGrid
              , children: [ R.text "New grid" ]
              }
            , R.button
              { onClick: capture_ $ send self StartTicking
              , children: [ R.text "Start" ]
              }
            , R.button
              { onClick: capture_ $ send self StopTicking
              , children: [ R.text "Stop" ]
              }
            , displayGameOfLife self 
        ]