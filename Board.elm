module Board (..) where

import Block exposing (Block)
import Color
import Dict exposing (Dict)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (down, flow, show)
import Tetromino exposing (Tetromino, Location)


type alias Board =
  Dict Location Block


new : List ( Location, Block ) -> Board
new =
  Dict.fromList


cols : Int
cols =
  10


rows : Int
rows =
  20


background : Form
background =
  let
    shape =
      rect ((toFloat cols) * Block.size) ((toFloat rows) * Block.size)

    border =
      outlined (solid Color.black) shape
  in
    group [ border, filled Color.black shape ]


addBlock : Location -> Block -> Form -> Form
addBlock ( row, col ) block form =
  let
    offSetX =
      -(toFloat (cols - 1)) / 2 * Block.size

    offSetY =
      -(toFloat (rows - 1)) / 2 * Block.size

    x =
      (toFloat col) * Block.size

    y =
      (toFloat row) * Block.size

    blockForm =
      Block.toForm block |> move ( offSetX + x, offSetY + y )
  in
    group [ form, blockForm ]


toForm : Board -> Form
toForm board =
  Dict.foldr addBlock background board


testForm : Form
testForm =
  addBlock ( 0, 0 ) (Block Color.blue) background


testForm' : Form
testForm' =
  addBlock ( 1, 0 ) (Block Color.red) testForm


testForm'' : Form
testForm'' =
  addBlock ( 0, 1 ) (Block Color.yellow) testForm'


testBoard : Board
testBoard =
  new
    [ ( ( 0, 0 ), Block Color.blue )
    , ( ( 0, 1 ), Block Color.yellow )
    , ( ( 1, 0 ), Block Color.red )
    , ( ( 1, 1 ), Block Color.green )
    ]


cumulativeSum : List Int -> List Int
cumulativeSum =
  List.scanl (+) 0


iota : Int -> List Int
iota n =
  List.repeat (n - 1) 1 |> cumulativeSum


fillRow : Int -> Block -> Board -> Board
fillRow row block board =
  let
    columns =
      iota cols

    rows =
      List.repeat cols row

    locations =
      List.map2 (,) rows columns

    blocks =
      List.repeat cols block

    filledRow =
      List.map2 (,) locations blocks |> new
  in
    Dict.union filledRow board


{-| Is the row complete?
-}
rowComplete : Int -> Board -> Bool
rowComplete row board =
  let
    -- get the blocks for a specific row
    blocks =
      Dict.filter (\( r, _ ) _ -> r == row) board
  in
    -- check to see if the number of blocks in our row
    -- matches the number of columns on the board
    Dict.size blocks == cols


{-| Remove a row from a board and shift all blocks above that row down
-}
clearRow : Int -> Board -> Board
clearRow row board =
  let
    shift ( r, c ) block newBoard =
      if (r < row) then
        (Dict.insert ( r, c ) block newBoard)
      else if (r > row) then
        (Dict.insert ( r - 1, c ) block newBoard)
      else
        newBoard
  in
    Dict.foldr shift Dict.empty board


{-| return number of lines cleared and the new board if
we have have cleared any
-}
clearLines : Board -> ( Int, Board )
clearLines =
  let
    clearLines' row lines board =
      if (row >= rows) then
        -- base case
        ( lines, board )
      else if (rowComplete row board) then
        clearLines' row (lines + 1) (clearRow row board)
      else
        clearLines' (row + 1) lines board
  in
    clearLines' 0 0


addTetromino : Tetromino -> Board -> Board
addTetromino { shape, block } board =
  let
    asBoard =
      List.map2 (,) shape (List.repeat 4 block) |> new
  in
    Dict.union asBoard board


inBounds : Tetromino -> Bool
inBounds { shape } =
  let
    checkLocation ( r, c ) =
      r >= 0 && c >= 0 && c < cols
  in
    List.all checkLocation shape


{-| Does the Tetromino intersect with any of the blocks on the board?
-}
isIntersecting : Tetromino -> Board -> Bool
isIntersecting { shape } board =
  let
    checkLocation location =
      Dict.member location board
  in
    List.any checkLocation shape


{-| is the given tetromino at a valid location?
-}
isValid : Tetromino -> Board -> Bool
isValid tetromino board =
  (inBounds tetromino) && not (isIntersecting tetromino board)


tetromino : Tetromino
tetromino =
  Tetromino.shift ( 0, 5 ) Tetromino.j


test : Board
test =
  new []


main : Graphics.Element.Element
main =
  flow
    down
    [ collage 600 600 [ toForm (addTetromino tetromino test) ]
    , show <| isValid tetromino test
    ]
