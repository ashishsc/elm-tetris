module Controller exposing (..)

import AnimationFrame
import Keyboard
import Time exposing (Time)


type Msg
    = Rotate
    | Shift ( Int, Int )
    | Tick Time
    | ToggleMusic
    | NoOp -- user pressed an unsupported key


keyCodeToMsg : Keyboard.KeyCode -> Msg
keyCodeToMsg keyCode =
    case keyCode of
        -- down
        40 ->
            Shift ( -1, 0 )

        -- left
        37 ->
            Shift ( 0, -1 )

        -- is this backwards?
        -- right
        39 ->
            Shift ( 0, 1 )

        -- is this backwards?
        -- up
        38 ->
            Rotate

        _ ->
            NoOp


subscriptions : Sub Msg
subscriptions =
    let
        ticks =
            AnimationFrame.diffs Tick

        keys =
            Keyboard.downs keyCodeToMsg
    in
    Sub.batch [ ticks, keys ]
