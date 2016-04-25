module State (..) where

import Board exposing (Board)
import Controller exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Tetromino exposing (Tetromino)
import Time exposing (Time)


type alias State =
  { falling : Tetromino
  , board : Board
  , time : Time
  , nextShift : Time
  , shiftDelay : Time
  }


defaultState : State
defaultState =
  { falling = Tetromino.shift startingShift Tetromino.j
  , board = Board.new []
  , time = 0
  , nextShift = Time.second
  , shiftDelay = Time.second
  }


startingShift : ( Int, Int )
startingShift =
  ( 20, 5 )


view : State -> Element
view state =
  let
    screenWidth =
      800

    screenHeight =
      600

    boardForm =
      Board.addTetromino state.falling state.board |> Board.toForm
  in
    collage screenWidth screenHeight [ boardForm ]


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


useIfValid : State -> State -> State
useIfValid current new =
  if Board.isValid new.falling new.board then
    new
  else
    current


tryKicks : List ( Int, Int ) -> State -> State -> State
tryKicks shifts current nextState =
  case shifts of
    [] ->
      current

    s :: rest ->
      let
        shifted =
          Tetromino.shift s nextState.falling
      in
        if Board.isValid shifted nextState.board then
          { nextState
            | falling = shifted
          }
        else
          tryKicks rest current nextState


wallKick : State -> State -> State
wallKick current nextState =
  let
    range =
      nextState.falling.cols // 2

    shifts =
      [1..range] |> List.concatMap (\n -> [ ( 0, n ), ( 0, -n ) ])
  in
    tryKicks shifts current nextState


floorKick : State -> State -> State
floorKick current nextState =
  let
    range =
      nextState.falling.rows // 2

    shifts =
      [1..range] |> List.map (\n -> ( n, 0 ))
  in
    tryKicks shifts current nextState


update : Input -> State -> State
update input state =
  let
    useIfValid' =
      useIfValid state
  in
    case input of
      Rotate ->
        let
          rotated =
            { state
              | falling = Tetromino.rotate state.falling
            }

          nextState =
            useIfValid' rotated

          -- Try various kicks if the state didn't change
          nextState' =
            if nextState == state then
              wallKick state rotated
            else
              nextState

          nextState'' =
            if nextState' == state then
              floorKick state rotated
            else
              nextState'
        in
          nextState''

      Shift amount ->
        useIfValid'
          { state
            | falling = Tetromino.shift amount state.falling
          }

      Tick delta ->
        useIfValid'
          <| checkTick
              { state
                | time = state.time + delta
              }


states : Signal State
states =
  Signal.foldp update defaultState inputs


main : Signal Element
main =
  Signal.map view states
