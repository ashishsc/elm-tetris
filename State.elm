module State (..) where

import Controller exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Tetromino exposing (Tetromino)


type alias State =
  { falling : Tetromino
  }


defaultState : State
defaultState =
  { falling = Tetromino.j }


view : State -> Element
view state =
  let
    screenWidth =
      800

    screenHeight =
      600

    fallingForm =
      Tetromino.toForm state.falling
  in
    collage screenWidth screenHeight [ fallingForm ]


update : Input -> State -> State
update input state =
  case input of
    Rotate ->
      { state
        | falling = Tetromino.rotate state.falling
      }

    Shift amount ->
      { state
        | falling = Tetromino.shift amount state.falling
      }


states : Signal State
states =
  Signal.foldp update defaultState inputs


main : Signal Element
main =
  Signal.map view states
