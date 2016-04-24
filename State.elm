module State (..) where

import Controller exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Tetromino exposing (Tetromino)
import Time exposing (Time)


type alias State =
  { falling : Tetromino
  , time : Time
  , nextShift : Time
  , shiftDelay : Time
  }


defaultState : State
defaultState =
  { falling = Tetromino.j
  , time = 0
  , nextShift = Time.second
  , shiftDelay = Time.second
  }


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


{-| if it is time to do a shift, shift the piece down and update the
scheduled nextShift time
-}
checkTick : State -> State
checkTick state =
  if (state.time < state.nextShift) then
    state
  else
    { state
      | falling = Tetromino.shift ( -1, 0 ) state.falling
      , nextShift = state.time + state.shiftDelay
    }


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

    Tick delta ->
      checkTick
        { state
          | time = state.time + delta
        }


states : Signal State
states =
  Signal.foldp update defaultState inputs


main : Signal Element
main =
  Signal.map view states
