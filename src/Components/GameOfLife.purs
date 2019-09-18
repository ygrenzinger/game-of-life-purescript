module Components.GameOfLife where
  
import Prelude

import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
-- import Effect (pureE) << why pureE from Effect can't be imported
import Effect.Console (log)
import Effect.Timer (clearTimeout, setInterval)
import GameOfLife (CellState(..), Position, createEmptyGrid, gridFrom, nextGeneration, switchState)
import React.Basic (Component, JSX, StateUpdate(..), createComponent, empty, make, runUpdate)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

component :: Component Props
component = createComponent "Game of Life"

type Props = { 
    size :: Int
}
      
data Action = NewGrid
    | StartRunning
    | StopRunning
    | Tick
    | SwitchState Position

gameOfLife :: Props -> JSX
gameOfLife = make component { initialState, didMount, render }
  where
    initialState = { gameOfLife: Nothing, intervalId: Nothing, running: false  }

    didMount = \self -> do
      intervalId <- setInterval 1000 do
        send self Tick
      void $ self.setState \s -> s { intervalId = Just intervalId }

    willUnmount = \self -> 
      case (self.state.intervalId) of
        Nothing -> log $ "" -- how to create an empty Effect Unit
        Just intervalId -> void $ clearTimeout intervalId       

    update self action = case action of
        NewGrid -> 
          UpdateAndSideEffects
            (self.state { gameOfLife = Just $ createEmptyGrid self.props.size })
            \_ -> log $ "Create new game of life"
        StartRunning -> 
          Update (self.state { running = true })
        StopRunning -> 
          Update (self.state { running = false })
        Tick -> 
          Update
          (self.state { gameOfLife = self.state.gameOfLife >>= \g ->
            if (self.state.running) then nextGeneration g else Just g })
        SwitchState position -> 
          UpdateAndSideEffects
            (self.state { gameOfLife = self.state.gameOfLife >>= \g -> switchState g position })
            \_ -> log $ "Making alive cell at : " <> show position

    send = runUpdate update

    displayCell self rowIndex columnIndex cellState = R.span
        { onClick: capture_ $ send self (SwitchState (Tuple rowIndex columnIndex))
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
              { onClick: capture_ $ send self StartRunning
              , children: [ R.text "Run" ]
              }
            , R.button
              { onClick: capture_ $ send self StopRunning
              , children: [ R.text "Pause" ]
              }
            , displayGameOfLife self 
        ]