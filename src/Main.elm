module Main exposing (main)

import Array
import Browser exposing (sandbox)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Html exposing (Html)
import Matrix exposing (Matrix)


main : Program () Model Msg
main =
    sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Matrix Bool


init : Model
init =
    Matrix.repeat 5 5 True



-- UPDATE


type Msg
    = ToggleLight Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLight toggleX toggleY ->
            Matrix.indexedMap (toggleLight toggleX toggleY) model


toggleLight : Int -> Int -> Int -> Int -> Bool -> Bool
toggleLight toggleX toggleY lightX lightY isOn =
    if lightX == toggleX && lightY == toggleY then
        not isOn

    else if lightX == toggleX && lightY == toggleY - 1 then
        not isOn

    else if lightX == toggleX && lightY == toggleY + 1 then
        not isOn

    else if lightX == toggleX - 1 && lightY == toggleY then
        not isOn

    else if lightX == toggleX + 1 && lightY == toggleY then
        not isOn

    else
        isOn



-- VIEW


view : Model -> Html Msg
view model =
    model
        |> rows
        |> List.indexedMap lightButtonRow
        |> column [ centerX, centerY, spacing 8 ]
        |> Element.layout [ Background.color darkGray ]


rows : Matrix a -> List (List a)
rows matrix =
    List.range 0 (Matrix.height matrix - 1)
        |> List.map (\y -> Matrix.getRow y matrix)
        |> List.map (Result.withDefault Array.empty)
        |> List.map Array.toList


lightButtonRow : Int -> List Bool -> Element Msg
lightButtonRow y =
    List.indexedMap (lightButton y) >> row [ centerX, spacing 8 ]


lightButton : Int -> Int -> Bool -> Element Msg
lightButton y x isOn =
    el
        [ Background.color <| lightColor isOn
        , width <| px 100
        , height <| px 100
        , onClick <| ToggleLight x y
        , Border.rounded 10
        ]
        none


lightColor : Bool -> Color
lightColor isOn =
    if isOn then
        brightGreen

    else
        dimGreen



-- COLORS


darkGray : Color
darkGray =
    rgb255 38 38 38


lightGray : Color
lightGray =
    rgb255 188 188 188


brightGreen : Color
brightGreen =
    rgba255 181 189 104 1


dimGreen : Color
dimGreen =
    rgba255 181 189 104 0.1
