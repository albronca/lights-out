module Main exposing (main)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
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
        , subscriptions = subscriptions
        }



-- MODEL


type GameState
    = Setup
    | Playing
    | Won


type alias Board =
    Matrix Bool


type alias Model =
    { gameState : GameState
    , board : Board
    , numSeedMoves : Int
    , numMovesMade : Int
    }


initialBoard : Board
initialBoard =
    Matrix.repeat 5 5 False


initialModel : Model
initialModel =
    { gameState = Setup
    , board = initialBoard
    , numSeedMoves = 0
    , numMovesMade = 0
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = ToggleLight Int Int
    | NewGame
    | SeedBoard (List ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLight toggleX toggleY ->
            case model.gameState of
                Playing ->
                    let
                        board =
                            toggleLight ( toggleX, toggleY ) model.board
                    in
                    ( { model
                        | board = board
                        , gameState = getGameState board
                        , numMovesMade = model.numMovesMade + 1
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SeedBoard coordList ->
            ( { model
                | board = toggleLights coordList model.board
                , gameState = Playing
                , numSeedMoves = List.length coordList
              }
            , Cmd.none
            )

        NewGame ->
            ( { initialModel | gameState = Setup }
            , Random.generate SeedBoard (coordListGenerator model.board)
            )


getGameState : Board -> GameState
getGameState board =
    if isBoardWon board then
        Won

    else
        Playing


isBoardWon : Board -> Bool
isBoardWon board =
    board == initialBoard


toggleLight : ( Int, Int ) -> Board -> Board
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


toggleLights : List ( Int, Int ) -> Board -> Board
toggleLights coordList board =
    coordList
        |> List.foldl toggleLight board


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    gameBoard model
        |> Element.layout
            [ Background.color darkGray
            , Font.color brightGreen
            , Font.size 16
            , Font.family
                [ Font.typeface "Courier"
                , Font.monospace
                ]
            ]


gameBoard : Model -> Element Msg
gameBoard model =
    model.board
        |> rows
        |> List.indexedMap lightButtonRow
        |> column
            [ centerX
            , centerY
            , spacing 8
            , paddingXY 32 0
            , onLeft <| sidebar model
            , onRight <| winText model
            ]


sidebar : Model -> Element Msg
sidebar model =
    column
        [ height fill, spacing 8 ]
        [ el [ Font.size 40 ] <| text "lights out"
        , Input.button [] { onPress = Just NewGame, label = text "new game" }
        , stats model

        -- TODO: add credits
        -- , Input.button [] { onPress = Nothing, label = text "credits" }
        -- TODO: add user auth
        --, Input.button [ alignBottom ] { onPress = Nothing, label = text "sign in" }
        ]


winText : Model -> Element Msg
winText model =
    column [ spacing 32 ]
        [ haiku
        , case model.gameState of
            Won ->
                text ":)"

            _ ->
                none
        ]


haiku : Element Msg
haiku =
    column [ spacing 16 ]
        [ text "a haiku:"
        , column []
            [ text "digital candles"
            , text "relit again and again"
            , text "by a ghostly flame"
            ]
        ]



-- TODO: add stats


stats : Model -> Element Msg
stats model =
    column
        [ alignBottom, spacing 8 ]
        [ text <| "moves:" ++ String.fromInt model.numMovesMade
        , text <| "goal:" ++ String.fromInt model.numSeedMoves
        ]


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
