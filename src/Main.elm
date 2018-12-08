module Main exposing (main)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import List.Extra
import Matrix exposing (Matrix)
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    Matrix Bool


initialModel : Model
initialModel =
    Matrix.repeat 5 5 False


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = ToggleLight Int Int
    | NewGame
    | ToggleLights (List ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLight toggleX toggleY ->
            ( toggleLight ( toggleX, toggleY ) model, Cmd.none )

        ToggleLights coordList ->
            ( toggleLights coordList model, Cmd.none )

        NewGame ->
            ( initialModel, Random.generate ToggleLights (coordListGenerator model) )


toggleLight : ( Int, Int ) -> Model -> Model
toggleLight ( toggleX, toggleY ) =
    Matrix.indexedMap
        (\lightX lightY isOn ->
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
        )


toggleLights : List ( Int, Int ) -> Model -> Model
toggleLights coordList model =
    coordList
        |> List.foldl toggleLight model


coordListGenerator : Matrix a -> Random.Generator (List ( Int, Int ))
coordListGenerator matrix =
    let
        ( width, height ) =
            ( Matrix.width matrix, Matrix.height matrix )

        ( maxX, maxY ) =
            ( width - 1, height - 1 )

        maxCount =
            width * height
    in
    Random.int 0 maxCount
        |> Random.andThen (\len -> Random.list len (coordGenerator maxX maxY))
        |> Random.map List.Extra.unique


coordGenerator : Int -> Int -> Random.Generator ( Int, Int )
coordGenerator maxX maxY =
    Random.pair (Random.int 0 maxX) (Random.int 0 maxY)



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color darkGray
        , Font.color brightGreen
        , Font.size 16
        , Font.family
            [ Font.typeface "Courier"
            , Font.monospace
            ]
        ]
        (row
            [ width fill, centerY ]
            [ gameBoard model ]
        )


menu : Element Msg
menu =
    column
        [ alignTop, paddingXY 32 0 ]
        [ el [ Font.size 32 ] <| text "lights out"
        , el [ Events.onClick NewGame ] <| text "new game"
        ]


gameBoard : Model -> Element Msg
gameBoard model =
    model
        |> rows
        |> List.indexedMap lightButtonRow
        |> column [ centerX, spacing 8, onLeft menu ]


rows : Matrix a -> List (List a)
rows matrix =
    List.range 0 (Matrix.height matrix - 1)
        |> List.map (\y -> Matrix.getRow y matrix)
        |> List.map (Result.withDefault Array.empty)
        |> List.map Array.toList


lightButtonRow : Int -> List Bool -> Element Msg
lightButtonRow y =
    List.indexedMap (lightButton y) >> row [ spacing 8 ]


lightButton : Int -> Int -> Bool -> Element Msg
lightButton y x isOn =
    el
        [ Background.color <| lightColor isOn
        , width <| px 100
        , height <| px 100
        , Events.onClick <| ToggleLight x y
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
