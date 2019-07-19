module Main exposing (main)

import Browser
import Game.Map exposing (Tile(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)


type alias Flags =
    ()


type alias Model =
    { seed : Int
    , depth : Int
    , chance : Float
    }


type Msg
    = Update Field String


type Field
    = Chance
    | Seed
    | Depth


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model 123456789 10 0.5, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update Seed str ->
            String.toInt str
                |> Maybe.map (\seed -> ( { model | seed = seed }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        Update Depth str ->
            String.toInt str
                |> Maybe.map (\depth -> ( { model | depth = depth }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        Update Chance str ->
            String.toFloat str
                |> Maybe.map (\chance -> ( { model | chance = chance }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view model =
    let
        size =
            25

        map =
            Game.Map.init
                { size = size
                , seed = model.seed
                , depth = model.depth
                , chance = Basics.max 0.0 (Basics.min 1.0 model.chance)
                }
    in
    div []
        [ div []
            [ label []
                [ strong [] [ Html.text "Seed" ]
                , input
                    [ Events.onInput (Update Seed)
                    , Html.Attributes.type_ "number"
                    , value (String.fromInt model.seed)
                    ]
                    []
                ]
            ]
        , div []
            [ label []
                [ strong [] [ Html.text "Depth" ]
                , input
                    [ Events.onInput (Update Depth)
                    , Html.Attributes.type_ "number"
                    , value (String.fromInt model.depth)
                    ]
                    []
                ]
            ]
        , div []
            [ label []
                [ strong [] [ Html.text "Chance" ]
                , input
                    [ Events.onInput (Update Chance)
                    , Html.Attributes.type_ "number"
                    , value (String.fromFloat model.chance)
                    ]
                    []
                ]
            ]
        , svg
            [ Attr.width (24 * size |> String.fromInt)
            , Attr.height (24 * size |> String.fromInt)
            , [ 0, 0, size, size ]
                |> List.map String.fromInt
                |> String.join " "
                |> Attr.viewBox
            ]
            (List.map
                (\y ->
                    List.map
                        (\x -> viewTile ( x, y ) (Game.Map.get ( x, y ) map))
                        (List.range 0 size)
                )
                (List.range 0 size)
                |> List.concat
            )
        ]


viewTile : ( Int, Int ) -> Tile -> Svg msg
viewTile ( x, y ) tile =
    let
        ( color, children ) =
            case tile of
                Grass ->
                    ( "#396", [] )

                Tree depth ->
                    ( "#274"
                    , [ Svg.node "text"
                            [ Attr.x "0.35"
                            , Attr.y "0.65"
                            , Attr.fontSize "0.5"
                            ]
                            [ Svg.text (String.fromInt depth) ]
                      ]
                    )

                Water depth ->
                    ( "#49f"
                    , [ Svg.node "text"
                            [ Attr.x "0.35"
                            , Attr.y "0.65"
                            , Attr.fontSize "0.5"
                            ]
                            [ Svg.text (String.fromInt depth) ]
                      ]
                    )
    in
    Svg.svg
        [ Attr.x (String.fromInt x)
        , Attr.y (String.fromInt y)
        , fill "white"
        ]
        (rect
            [ Attr.x "0"
            , Attr.y "0"
            , Attr.width "1"
            , Attr.height "1"
            , fill color
            ]
            []
            :: children
        )
