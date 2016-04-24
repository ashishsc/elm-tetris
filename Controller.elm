module Controller (..) where

import Graphics.Element exposing (Element, show)
import Keyboard exposing (arrows)
import Signal exposing (Signal)
import Time exposing (Time, fps)


type Input
  = Rotate
  | Shift ( Int, Int )
  | Tick Time


arrowsToInput : { x : Int, y : Int } -> Input
arrowsToInput { x, y } =
  if (y == 1) then
    Rotate
  else
    Shift ( y, x )


inputs : Signal Input
inputs =
  let
    ticks =
      Signal.map Tick (fps 30)

    keys =
      Signal.map arrowsToInput arrows
  in
    Signal.merge ticks keys


main : Signal Element
main =
  Signal.map show inputs
