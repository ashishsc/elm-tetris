module Block exposing (..)

import Collage exposing (..)
import Color exposing (Color)


type alias Block =
    { color : Color }


size : Float
size =
    25


toForm : Block -> Form
toForm block =
    let
        shape =
            square size

        border =
            outlined (solid Color.black) shape
    in
    group [ filled block.color shape, border ]
