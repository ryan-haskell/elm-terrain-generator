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
    , ocean : Settings
    , forest : Settings
    }


type alias Settings =
    { count : Int
    , depth : Int
    , chance : Int
    }


type Biome
    = Ocean
    | Forest


type Msg
    = UpdateSeed String
    | Update Biome Field String


type Field
    = Chance
    | Count
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
    ( Model 123456789 (Settings 3 10 50) (Settings 3 10 50)
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSeed value ->
            String.toInt value
                |> Maybe.map (\seed -> ( { model | seed = seed }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        Update biome field value ->
            String.toInt value
                |> Maybe.map
                    (\int ->
                        case biome of
                            Forest ->
                                { model | forest = updateBiomeSettings model.forest field int }

                            Ocean ->
                                { model | ocean = updateBiomeSettings model.ocean field int }
                    )
                |> Maybe.map (\m -> ( m, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )


updateBiomeSettings settings field int =
    let
        updateIntField fn =
            fn settings
    in
    case field of
        Count ->
            updateIntField (\s -> { s | count = int })

        Depth ->
            updateIntField (\s -> { s | depth = int })

        Chance ->
            updateIntField (\s -> { s | chance = int })



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        size =
            48

        map =
            Game.Map.init
                { size = size
                , seed = model.seed
                , tiles =
                    [ { tile = Tree
                      , count = model.forest.count
                      , depth = model.forest.depth
                      , chance = toFloat (Basics.max 0 (Basics.min 100 model.forest.chance)) / 100
                      }
                    , { tile = Water
                      , count = model.ocean.count
                      , depth = model.ocean.depth
                      , chance = toFloat (Basics.max 0 (Basics.min 100 model.ocean.chance)) / 100
                      }
                    ]
                }
    in
    div [ Html.Attributes.style "display" "flex" ]
        [ div []
            [ div []
                [ label []
                    [ strong [] [ Html.text "Seed" ]
                    , input
                        [ Events.onInput UpdateSeed
                        , Html.Attributes.type_ "number"
                        , value (String.fromInt model.seed)
                        ]
                        []
                    ]
                ]
            , div [ Html.Attributes.style "display" "flex" ]
                [ viewSettings "Ocean" model.ocean Ocean
                , viewSettings "Forest" model.forest Forest
                ]
            ]
        , svg
            [ Attr.width (16 * size |> String.fromInt)
            , Attr.height (16 * size |> String.fromInt)
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


viewSettings : String -> Settings -> Biome -> Html Msg
viewSettings title settings biome =
    div [ Html.Attributes.style "margin-right" "1rem" ]
        [ h4 [] [ Html.text title ]
        , div []
            [ label []
                [ strong [] [ Html.text "Count" ]
                , input
                    [ Events.onInput (Update biome Count)
                    , Html.Attributes.type_ "number"
                    , value (String.fromInt settings.count)
                    ]
                    []
                ]
            ]
        , div []
            [ label []
                [ strong [] [ Html.text "Depth" ]
                , input
                    [ Events.onInput (Update biome Depth)
                    , Html.Attributes.type_ "number"
                    , value (String.fromInt settings.depth)
                    ]
                    []
                ]
            ]
        , div []
            [ label []
                [ strong [] [ Html.text "Chance" ]
                , input
                    [ Events.onInput (Update biome Chance)
                    , Html.Attributes.type_ "number"
                    , value (String.fromInt settings.chance)
                    ]
                    []
                ]
            ]
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
