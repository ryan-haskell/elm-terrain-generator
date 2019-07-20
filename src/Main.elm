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
    , size : Int
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
    | UpdateSize String
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
    ( Model 123456771 25 (Settings 6 40 66) (Settings 37 6 70)
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

        UpdateSize value ->
            String.toInt value
                |> Maybe.map (\size -> ( { model | size = size }, Cmd.none ))
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


percentify : Int -> Float
percentify chance =
    toFloat (Basics.max 0 (Basics.min 100 chance)) / 100


view : Model -> Html Msg
view model =
    let
        map =
            Game.Map.init
                { size = Basics.min 50 (Basics.max 10 model.size)
                , seed = model.seed
                , tile = Grass
                , tiles =
                    [ { tile = always Tree
                      , count = model.forest.count
                      , depth = model.forest.depth
                      , chance = percentify model.forest.chance
                      }
                    , { tile = always Water
                      , count = model.ocean.count
                      , depth = model.ocean.depth
                      , chance = percentify model.ocean.chance
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
            , div []
                [ label []
                    [ strong [] [ Html.text "Size" ]
                    , input
                        [ Events.onInput UpdateSize
                        , Html.Attributes.type_ "number"
                        , value (String.fromInt model.size)
                        ]
                        []
                    ]
                ]
            , div []
                [ viewSettings "Ocean" model.ocean Ocean
                , viewSettings "Forest" model.forest Forest
                ]
            ]
        , svg
            [ Attr.width (12 * model.size |> String.fromInt)
            , Attr.height (12 * model.size |> String.fromInt)
            , [ 0, 0, model.size, model.size ]
                |> List.map String.fromInt
                |> String.join " "
                |> Attr.viewBox
            ]
            (List.map
                (\y ->
                    List.map
                        (\x -> viewTile ( x, y ) (Game.Map.get ( x, y ) map))
                        (List.range 0 model.size)
                )
                (List.range 0 model.size)
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
        color =
            case tile of
                Grass ->
                    "#396"

                Tree ->
                    "#274"

                Water ->
                    "#49f"
    in
    Svg.svg
        [ Attr.x (String.fromInt x)
        , Attr.y
            (String.fromFloat
                (if modBy 2 x == 0 then
                    toFloat y

                 else
                    toFloat y + 0.5
                )
            )
        , fill "white"
        ]
        [ hexagon color
        ]


hexagon : String -> Svg msg
hexagon color =
    polygon
        [ points "0,0.5 0.35,0 1,0 1.35,0.5 1,1 0.35,1"
        , fill color
        ]
        []
