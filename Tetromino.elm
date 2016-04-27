module Tetromino (..) where

import Block exposing (Block)
import Color exposing (Color)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Random exposing (Generator)


{-| row, col
-}
type alias Location =
  ( Int, Int )


type alias Tetromino =
  { shape : List Location
  , block : Block
  , pivot :
      { r : Float
      , c : Float
      }
  , rows : Int
  , cols : Int
  }


toForm : Tetromino -> Form
toForm { shape, block } =
  let
    form =
      Block.toForm block

    translate ( row, col ) =
      move
        ( (toFloat col) * Block.size
        , (toFloat row) * Block.size
        )
        form

    forms =
      List.map translate shape
  in
    group forms


i : Tetromino
i =
  { shape =
      [ ( 1, 0 )
      , ( 0, 0 )
      , ( -1, 0 )
      , ( -2, 0 )
      ]
  , block = Block Color.lightBlue
  , pivot = { r = -0.5, c = 0.5 }
  , rows = 4
  , cols = 1
  }


j : Tetromino
j =
  { shape =
      [ ( 1, 0 )
      , ( 0, 0 )
      , ( -1, -1 )
      , ( -1, 0 )
      ]
  , block = Block Color.blue
  , pivot = { r = 0.0, c = 0.0 }
  , rows = 3
  , cols = 2
  }


l : Tetromino
l =
  { shape =
      [ ( 1, 0 )
      , ( 0, 0 )
      , ( -1, 0 )
      , ( -1, 1 )
      ]
  , block = Block Color.orange
  , pivot = { r = 0.0, c = 0.0 }
  , rows = 3
  , cols = 2
  }


z : Tetromino
z =
  { shape =
      [ ( 1, -1 )
      , ( 1, 0 )
      , ( 0, 0 )
      , ( 0, 1 )
      ]
  , block = Block Color.red
  , pivot = { r = 0.0, c = 0.0 }
  , rows = 2
  , cols = 3
  }


s : Tetromino
s =
  { shape =
      [ ( 0, 0 )
      , ( 0, 1 )
      , ( -1, -1 )
      , ( -1, 0 )
      ]
  , block = Block Color.green
  , pivot = { r = 0.0, c = 0.0 }
  , rows = 2
  , cols = 3
  }


t : Tetromino
t =
  { shape =
      [ ( 0, -1 )
      , ( 0, 0 )
      , ( 0, 1 )
      , ( -1, 0 )
      ]
  , block = Block Color.purple
  , pivot = { r = 0.0, c = 0.0 }
  , rows = 2
  , cols = 3
  }


o : Tetromino
o =
  { shape =
      [ ( 0, 0 )
      , ( 0, 1 )
      , ( -1, 0 )
      , ( -1, 1 )
      ]
  , block = Block Color.yellow
  , pivot = { r = -0.5, c = 0.5 }
  , rows = 2
  , cols = 2
  }


drawPivot : Tetromino -> Form
drawPivot { pivot } =
  let
    dot =
      circle 5 |> filled Color.black

    translate =
      move ( pivot.c * Block.size, pivot.r * Block.size )
  in
    translate dot


rotateLocation : { r : Float, c : Float } -> Float -> Location -> Location
rotateLocation pivot angle ( row, col ) =
  let
    rowOrigin =
      (toFloat row) - pivot.r

    colOrigin =
      (toFloat col) - pivot.c

    ( s, c ) =
      ( sin (angle), cos (angle) )

    rowRotated =
      rowOrigin * c - colOrigin * s

    colRotated =
      rowOrigin * s + colOrigin * c
  in
    ( round <| rowRotated + pivot.r, round <| colRotated + pivot.c )


rotate : Tetromino -> Tetromino
rotate tetromino =
  let
    rotateHelper =
      rotateLocation tetromino.pivot (degrees 90)

    newShape =
      List.map rotateHelper tetromino.shape
  in
    { tetromino
      | shape = newShape
      , rows = tetromino.cols
      , cols = tetromino.rows
    }


shift : ( Int, Int ) -> Tetromino -> Tetromino
shift ( rows, cols ) tetromino =
  let
    shiftHelper ( row, col ) =
      ( row + rows, col + cols )

    newShape =
      List.map shiftHelper tetromino.shape

    pivot' =
      { r = tetromino.pivot.r + (toFloat rows)
      , c = tetromino.pivot.c + (toFloat cols)
      }
  in
    { tetromino
      | shape = newShape
      , pivot = pivot'
    }


tetromino : Tetromino
tetromino =
  shift ( -3, 0 ) o


zeroToOne : Generator Float
zeroToOne =
  Random.float 0 1


bag : Generator (List Tetromino)
bag =
  let
    -- generate 7 random numbers
    weights =
      Random.list 7 zeroToOne
  in
    Random.map shuffleBag weights


{-| returns a list of tetrominos based
on the weights assigned, highest weights returned
first
-}
shuffleBag : List Float -> List Tetromino
shuffleBag weights =
  let
    tetrominoes =
      [ i, o, j, l, z, s, t ]

    weighted =
      List.map2 (,) weights tetrominoes

    sorted =
      List.sortBy fst weighted
  in
    List.map snd sorted


main : Element
main =
  Random.generate bag (Random.initialSeed 43)
    |> fst
    |> List.map toForm
    |> List.map (\f -> collage 80 80 [ f ])
    |> flow right
