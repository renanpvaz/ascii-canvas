module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (..)
import List exposing (indexedMap, map, repeat)
import Json.Decode as Json


type alias Row =
    List Cell


type alias Cell =
    { cell : String, color : String }


type alias Table =
    List Row


type alias CellPos =
    { row : Maybe String, cell : Maybe String }


type alias CellPos2 =
    Maybe ( String, String )


type alias Model =
    { table : Table
    , drawing : Bool
    , showGrid : Bool
    , char : String
    , color : String
    }


type Msg
    = StartDrawing
    | StopDrawing
    | Draw CellPos
    | SetColor String
    | ShowGrid
    | HideGrid


makeTable : Int -> Int -> Table
makeTable rows cells =
    repeat rows (repeat cells (Cell "" "black"))


initModel : Model
initModel =
    { table = makeTable 40 80
    , drawing = False
    , showGrid = True
    , char = "$"
    , color = "black"
    }


mapAt f index =
    indexedMap
        (\i x ->
            if i == index then
                f x
            else
                x
        )


updateTable : Int -> Int -> String -> String -> Table -> Table
updateTable row cell char color table =
    mapAt
        (mapAt (\c -> { color = color, cell = char }) cell)
        row
        table


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetColor color ->
            { model | color = color }

        StartDrawing ->
            { model | drawing = True }

        StopDrawing ->
            { model | drawing = False }

        ShowGrid ->
            { model | showGrid = True }

        HideGrid ->
            { model | showGrid = False }

        Draw pos ->
            if model.drawing then
                case pos.row of
                    Just row ->
                        case pos.cell of
                            Just cell ->
                                { model
                                    | table =
                                        updateTable
                                            (Result.withDefault 0 (String.toInt row))
                                            (Result.withDefault 0 (String.toInt cell))
                                            model.char
                                            model.color
                                            model.table
                                }

                            _ ->
                                model

                    _ ->
                        model
            else
                model


decodePos : Decoder CellPos
decodePos =
    field "target" <|
        Json.map2 CellPos
            (at [ "dataset", "row" ] (nullable string))
            (at [ "dataset", "cell" ] (nullable string))


cell : Int -> Int -> Cell -> Html Msg
cell row cellN cell =
    div
        [ class "cell"
        , style [ ( "color", cell.color ) ]
        , attribute "data-row" (toString row)
        , attribute "data-cell" (toString cellN)
        , on "mousemove" (Json.map Draw decodePos)
        ]
        [ text cell.cell ]


row : Int -> Row -> Html Msg
row rowN =
    indexedMap (cell rowN) >> div [ class "row" ]


table : Model -> Html Msg
table model =
    .table model
        |> indexedMap row
        |> div
            [ classList
                [ ( "table", True )
                , ( "table--with-grid", model.showGrid )
                ]
            ]


view : Model -> Html Msg
view model =
    div [ onMouseDown StartDrawing, onMouseUp StopDrawing ]
        [ h1 [] [ text "ASCII ART TOOL" ]
        , table model
        , footer []
            [ input
                [ type_ "color"
                , Html.Attributes.value model.color
                , onInput SetColor
                ]
                []
            , input
                [ type_ "checkbox"
                , onCheck
                    (\checked ->
                        if checked then
                            ShowGrid
                        else
                            HideGrid
                    )
                , checked model.showGrid
                ]
                []
            , span [] [ text "show grid" ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
