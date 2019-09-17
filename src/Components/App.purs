module Components.App where

import Prelude

import Components.GameOfLife (gameOfLife)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as R

component :: Component Unit
component = createComponent "App"

app :: JSX
app = unit # makeStateless component \_ ->
  R.div_
    [ R.h1_ [ R.text "Game of Life" ]
    , gameOfLife { size: 30 }
    ]
