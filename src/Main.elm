port module Main exposing (..)

import Board exposing (Board)
import Collage exposing (..)
import Controller exposing (Msg(..), subscriptions)
import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Random exposing (Generator, Seed)
import Score exposing (Score)
import Tetromino exposing (Tetromino)
import Time exposing (Time)


port toggleMusic : Bool -> Cmd msg


type alias State =
    { falling : Tetromino
    , score : Score
    , musicEnabled : Bool
    , seed : Seed
    , bag : List Tetromino
    , board : Board
    , time : Time
    , nextShift : Time
    , shiftDelay : Time
    }


initialSeed : Int
initialSeed =
    42


shiftDt : Time
shiftDt =
    Time.millisecond * 100


maxSpeed : Time
maxSpeed =
    Time.millisecond * 100


init : ( State, Cmd Msg )
init =
    let
        ( bag, seed ) =
            Random.step Tetromino.bag (Random.initialSeed initialSeed)

        falling =
            List.head bag |> Maybe.withDefault Tetromino.i

        bag_ =
            List.drop 1 bag
    in
    ( { falling = Tetromino.shift startingShift falling
      , score = 0
      , musicEnabled = True
      , seed = seed
      , bag = bag_
      , board = Board.new []
      , time = 0
      , nextShift = Time.second
      , shiftDelay = Time.second
      }
    , Cmd.none
    )


startingShift : ( Int, Int )
startingShift =
    ( 20, 5 )


view : State -> Html Msg
view state =
    let
        screenWidth =
            800

        screenHeight =
            600

        boardForm =
            Board.addTetromino state.falling state.board |> Board.toForm

        scoreForm =
            Score.toForm state.score |> move ( -160, 215 )

        music =
            let
                unicodeIcon =
                    if state.musicEnabled then
                        "🔊"
                    else
                        "🔇"
            in
            Html.div [ Attrs.class "music" ]
                [ Html.button
                    [ onClick ToggleMusic
                    , Attrs.classList [ ( "muted", not state.musicEnabled ) ]
                    ]
                    [ Html.text ("Toggle Music " ++ unicodeIcon) ]
                , Html.audio
                    [ Attrs.autoplay True
                    , Attrs.src "tetris.mp3"
                    , Attrs.loop True
                    , Attrs.id "music"
                    ]
                    []
                ]

        gameView =
            collage screenWidth screenHeight [ boardForm, scoreForm ]
                |> Element.toHtml
    in
    Html.div [] [ music, gameView ]


{-| Update the state's bag with a new bag if it is empty
otherwise, just return the existing bag
-}
checkBag : State -> State
checkBag state =
    if not (List.isEmpty state.bag) then
        state
    else
        let
            ( bag, seed ) =
                Random.step Tetromino.bag state.seed
        in
        { state | bag = bag, seed = seed }


{-| Take the next tetromino and add it to the state
if a bag is empty, a new one will be generated
-}
nextTetromino : State -> State
nextTetromino state =
    let
        state_ =
            checkBag state

        nextFalling =
            List.head state_.bag
                |> Maybe.withDefault Tetromino.i
                |> Tetromino.shift startingShift

        nextBag =
            List.drop 1 state_.bag

        ( lines, nextBoard ) =
            Board.addTetromino state_.falling state_.board |> Board.clearLines

        nextScore =
            Score.calculate lines state_.score

        shorterDelay =
            state.shiftDelay - shiftDt

        -- If the user has scored, ramp up until we hit maximum difficulty
        nextShiftDelay =
            if (state.score == nextScore) || (shorterDelay <= maxSpeed) then
                state.shiftDelay
            else
                shorterDelay
    in
    { state_
        | falling = nextFalling
        , bag = nextBag
        , board = nextBoard
        , score = nextScore
        , shiftDelay = nextShiftDelay
    }


{-| if it is time to do a shift, shift the piece down and update the
scheduled nextShift time. If the tetronomino has reached the bottom,
grab the nextTetromino from the bag
-}
checkTick : State -> State
checkTick state =
    if state.time < state.nextShift then
        state
    else
        let
            shifted =
                Tetromino.shift ( -1, 0 ) state.falling

            nextShift =
                state.time + state.shiftDelay

            isValid =
                Board.isValid shifted state.board

            state_ =
                if isValid then
                    { state | falling = shifted }
                else
                    nextTetromino state
        in
        { state_ | nextShift = nextShift }


{-| check to see if the given state is valid,
if it is return it, otherise return the old state
-}
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
            List.range 1 range |> List.concatMap (\n -> [ ( 0, n ), ( 0, -n ) ])
    in
    tryKicks shifts current nextState


floorKick : State -> State -> State
floorKick current nextState =
    let
        range =
            nextState.falling.rows // 2

        shifts =
            List.range 1 range |> List.map (\n -> ( n, 0 ))
    in
    tryKicks shifts current nextState


update : Msg -> State -> ( State, Cmd Msg )
update input state =
    let
        useIfValid_ =
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
                    useIfValid_ rotated

                -- Try various kicks if the state didn't change
                nextState_ =
                    if nextState == state then
                        wallKick state rotated
                    else
                        nextState

                nextState__ =
                    if nextState_ == state then
                        floorKick state rotated
                    else
                        nextState_
            in
            ( nextState__, Cmd.none )

        Shift amount ->
            let
                nextState =
                    useIfValid_
                        { state
                            | falling = Tetromino.shift amount state.falling
                        }
            in
            ( nextState, Cmd.none )

        Tick delta ->
            let
                nextState =
                    useIfValid_ <|
                        checkTick
                            { state
                                | time = state.time + delta
                            }
            in
            ( nextState, Cmd.none )

        ToggleMusic ->
            ( { state | musicEnabled = not state.musicEnabled }, toggleMusic state.musicEnabled )

        NoOp ->
            ( state, Cmd.none )


main : Program Never State Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> subscriptions
        }
