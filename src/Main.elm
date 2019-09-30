module Main exposing (Board, GameStatus(..), Hole, Model, Msg(..), Peg, Position, Range, board, gameStatus, initModel, main, update, view)

import Browser
import Element exposing (Element, OnGrid, button, cell, circle, column, empty, grid, layout, spacer, text)
import Element.Attributes exposing (center, padding, px, spacing)
import Element.Events exposing (onClick)
import Html exposing (Html)
import List exposing (any, filter, length, map, maximum, member, minimum, partition, range, repeat)
import List.Extra exposing (lift2)
import Style exposing (StyleSheet, rgba, style, styleSheet)
import Style.Border exposing (rounded)
import Style.Color exposing (background, border)
import Style.Font exposing (size)
import Style.Shadow exposing (glow)

main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, update = update, view = view }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Board =
    List Position


type alias Peg =
    Position


type alias Hole =
    Position


type alias Model =
    { holes : List Hole, pegs : List Peg, selected : Maybe Peg }


type Msg
    = Select Peg
    | Move Hole
    | Restart


type alias Range =
    { xmin : Int
    , xmax : Int
    , ymin : Int
    , ymax : Int
    }


type GameStatus
    = Win
    | Lose
    | Continue


board : List Position
board =
    let
        r =
            range -3 3
    in
    filter (\pos -> abs pos.x + abs pos.y <= 4) (lift2 (\i j -> { x = i, y = j }) r r)


initModel : Model
initModel =
    let
        ( h, p ) =
            partition (\pos -> pos == { x = 0, y = 1 }) board
    in
    { pegs = p, holes = h, selected = Nothing }


gameStatus : Model -> GameStatus
gameStatus model =
    let
        movable peg =
            (member { x = peg.x + 1, y = peg.y } model.pegs
                && member { x = peg.x + 2, y = peg.y } model.holes
            )
                || (member { x = peg.x - 1, y = peg.y } model.pegs
                        && member { x = peg.x - 2, y = peg.y } model.holes
                   )
                || (member { x = peg.x, y = peg.y + 1 } model.pegs
                        && member { x = peg.x, y = peg.y + 2 } model.holes
                   )
                || (member { x = peg.x, y = peg.y - 1 } model.pegs
                        && member { x = peg.x, y = peg.y - 2 } model.holes
                   )
    in
    if length model.pegs == 1 then
        Win

    else if any movable model.pegs then
        Continue

    else
        Lose


rangeBoard : { xmin : Int, xmax : Int, ymin : Int, ymax : Int }
rangeBoard =
    let
        xs =
            map (\e -> e.x) (initModel.holes ++ initModel.pegs)

        xmin =
            minimum xs

        xmax =
            maximum xs

        ys =
            map (\e -> e.y) (initModel.holes ++ initModel.pegs)

        ymin =
            minimum ys

        ymax =
            maximum ys
    in
    { xmin = Maybe.withDefault 0 xmin
    , xmax = Maybe.withDefault 0 xmax
    , ymin = Maybe.withDefault 0 ymin
    , ymax = Maybe.withDefault 0 ymax
    }


type Styles
    = PegStyle PegStyles
    | HoleStyle
    | GridStyle
    | ButtonStyle
    | MessageStyle


type PegStyles
    = Selected
    | NotSelected


ss : StyleSheet Styles variation
ss =
    styleSheet
        [ style (PegStyle Selected) [ background (rgba 0 0 1 1), glow (rgba 0 0 0 1) 1 ]
        , style (PegStyle NotSelected) [ background (rgba 0 1 0 1), glow (rgba 0 0 0 1) 1 ]
        , style HoleStyle [ background (rgba 0 0 0 0), glow (rgba 0 0 0 1) 1 ]
        , style MessageStyle [ size 32 ]
        , style ButtonStyle [ rounded 10.0 ]
        ]


drawPeg : Maybe Peg -> Peg -> OnGrid (Element Styles variation Msg)
drawPeg selected peg =
    let
        style =
            case selected of
                Nothing ->
                    NotSelected

                Just p ->
                    if peg == p then
                        Selected

                    else
                        NotSelected
    in
    cell
        { start = ( peg.x - rangeBoard.xmin, peg.y - rangeBoard.ymin )
        , width = 1
        , height = 1
        , content = circle 20 (PegStyle style) [ onClick (Select peg) ] empty
        }


drawHole : Hole -> OnGrid (Element Styles variation Msg)
drawHole hole =
    cell
        { start = ( hole.x - rangeBoard.xmin, hole.y - rangeBoard.ymin )
        , width = 1
        , height = 1
        , content = circle 20 HoleStyle [ onClick (Move hole) ] empty
        }


update msg model =
    case msg of
        Restart ->
            initModel

        Select peg ->
            { model | selected = Just peg }

        Move hole ->
            case model.selected of
                Nothing ->
                    model

                Just peg ->
                    let
                        mid =
                            { x = (peg.x + hole.x) // 2, y = (peg.y + hole.y) // 2 }

                        admissible =
                            ((peg.x == hole.x && abs (peg.y - hole.y) == 2)
                                || (peg.y == hole.y && abs (peg.x - hole.x) == 2)
                            )
                                && member mid model.pegs
                    in
                    if admissible then
                        { model
                            | pegs = filter (\p -> p /= peg && p /= mid) model.pegs ++ [ hole ]
                            , holes = filter (\h -> h /= hole) model.holes ++ [ peg, mid ]
                            , selected = Nothing
                        }

                    else
                        model


view : Model -> Html Msg
view model =
    layout ss <|
        case gameStatus model of
            Continue ->
                grid GridStyle
                    [ padding 10 ]
                    { columns = repeat (rangeBoard.xmax - rangeBoard.xmin + 1) (px 50)
                    , rows = repeat (rangeBoard.ymax - rangeBoard.ymin + 1) (px 50)
                    , cells = map (drawPeg model.selected) model.pegs ++ map drawHole model.holes
                    }

            Win ->
                column MessageStyle
                    [ padding 100, spacing 20, center ]
                    [ text "Gagné"
                    , spacer 1
                    , button ButtonStyle [ padding 10, onClick Restart ] (text "Rejouer")
                    ]

            Lose ->
                column MessageStyle
                    [ padding 100, spacing 20, center ]
                    [ text "Perdu"
                    , spacer 1
                    , button ButtonStyle [ padding 10, onClick Restart ] (text "Rejouer")
                    ]
