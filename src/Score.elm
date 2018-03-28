module Score exposing (..)

import Collage exposing (..)
import Color exposing (Color)
import Text


type alias Score =
    Int


size : Float
size =
    70


scoreMultiplier : Int
scoreMultiplier =
    10


calculate : Int -> Score -> Score
calculate linesCleared oldScore =
    linesCleared * scoreMultiplier + oldScore


scoreTextStyle : Text.Style
scoreTextStyle =
    let
        def =
            Text.defaultStyle
    in
    { def | color = Color.white }


toForm : Score -> Form
toForm score =
    let
        shape =
            filled Color.black (square size)

        scoreForm =
            score |> toString |> Text.fromString |> Text.style scoreTextStyle |> text
    in
    group [ shape, scoreForm ]
