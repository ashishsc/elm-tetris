module Score (..) where

import Color exposing (Color)
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (..)
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


main : Element
main =
  collage 400 400 [ toForm 20 ]
