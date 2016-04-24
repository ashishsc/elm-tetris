module Controller (..) where

import Graphics.Element exposing (Element, show)
import Keyboard exposing (arrows)
import Signal exposing (Signal)


type Input
  = Rotate
  | Shift ( Int, Int )


arrowsToInput : { x : Int, y : Int } -> Input
arrowsToInput { x, y } =
  if (y == 1) then
    Rotate
  else
    Shift ( y, x )


inputs : Signal Input
inputs =
  Signal.map arrowsToInput arrows


main : Signal Element
main =
  Signal.map show inputs
