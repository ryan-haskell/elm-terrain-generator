module Main exposing (main)

import Browser
import Configuration exposing (Configuration)
import Game.Map exposing (Tile(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as D exposing (Decoder)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Svg.Events


type alias Flags =
    ()


type alias Model =
    { json : String
    , offsetX : Int
    , offsetY : Int
    }


type Msg
    = UpdateJson String
    | OnKeyDown Direction


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
    ( Model """{
    "seed": 0,
    "size": 24,
    "water": { "count": 75, "depth": 4, "chance": 75 },
    "trees": { "count": 75, "depth": 12, "chance": 75 },
    "towns": { "count": 6, "depth": 32, "chance": 65 },
    "villages": { "count": 12, "depth": 4, "chance": 65 }
}""" 0 0
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        UpdateJson json ->
            { model | json = json }

        OnKeyDown dir ->
            case dir of
                Up ->
                    { model | offsetY = model.offsetY - 1 }

                Down ->
                    { model | offsetY = model.offsetY + 1 }

                UpLeft ->
                    { model
                        | offsetX = model.offsetX - 1
                        , offsetY = model.offsetY - 1
                    }

                UpRight ->
                    { model
                        | offsetX = model.offsetX + 1
                        , offsetY = model.offsetY - 1
                    }

                DownLeft ->
                    { model
                        | offsetX = model.offsetX - 1
                        , offsetY = model.offsetY + 1
                    }

                DownRight ->
                    { model
                        | offsetX = model.offsetX + 1
                        , offsetY = model.offsetY + 1
                    }
    , Cmd.none
    )



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
    div []
        [ div
            []
            [ textarea
                [ Events.onInput UpdateJson
                , value model.json
                , Html.Attributes.style "min-width" "25rem"
                , Html.Attributes.style "min-height" "7rem"
                , Html.Attributes.style "font-family" "monospace"
                ]
                []
            ]
        , p [] [ Html.text "Click on the map, and use QWEASD to move around" ]
        , case Configuration.parse model.json of
            Nothing ->
                Html.text "Couldn't understand JSON..."

            Just config ->
                let
                    map =
                        Game.Map.init
                            { size = config.size
                            , seed = config.seed
                            , tile = Grass
                            , tiles =
                                [ { tile = Tree
                                  , count = round <| percentify config.trees.count * toFloat config.size
                                  , depth = config.trees.depth
                                  , chance = percentify config.trees.chance
                                  }
                                , { tile = Water
                                  , count = round <| percentify config.water.count * toFloat config.size
                                  , depth = config.water.depth
                                  , chance = percentify config.water.chance
                                  }
                                , { tile = Town
                                  , count = round <| percentify config.towns.count * toFloat config.size
                                  , depth = config.towns.depth
                                  , chance = percentify config.towns.chance
                                  }
                                , { tile = Village
                                  , count = round <| percentify config.villages.count * toFloat config.size
                                  , depth = config.villages.depth
                                  , chance = percentify config.villages.chance
                                  }
                                ]
                            }
                in
                button [ Events.on "keydown" (D.map OnKeyDown keyDecoder) ]
                    [ svg
                        [ Attr.width (12 * config.size |> String.fromInt)
                        , Attr.height (12 * config.size |> String.fromInt)
                        , [ 0, 0, config.size, config.size ]
                            |> List.map String.fromInt
                            |> String.join " "
                            |> Attr.viewBox
                        ]
                        (List.map
                            (\y ->
                                List.map
                                    (\x ->
                                        viewTile
                                            ( x, y )
                                            ( modBy config.size (x - model.offsetX)
                                            , modBy config.size (y - model.offsetY)
                                            )
                                            (Game.Map.get ( x, y ) map)
                                    )
                                    (List.range 0 (config.size - 1))
                            )
                            (List.range 0 (config.size - 1))
                            |> List.concat
                        )
                    ]
        ]


type Direction
    = Up
    | Down
    | UpLeft
    | UpRight
    | DownLeft
    | DownRight


keyDecoder : Decoder Direction
keyDecoder =
    D.field "key" D.string
        |> D.andThen
            (\str ->
                case str of
                    "w" ->
                        D.succeed Up

                    "s" ->
                        D.succeed Down

                    "q" ->
                        D.succeed UpLeft

                    "e" ->
                        D.succeed UpRight

                    "a" ->
                        D.succeed DownLeft

                    "d" ->
                        D.succeed DownRight

                    _ ->
                        D.fail "Don't know dat."
            )


viewTile : ( Int, Int ) -> ( Int, Int ) -> Tile -> Svg Msg
viewTile ( realX, realY ) ( x, y ) tile =
    let
        color =
            case tile of
                Grass ->
                    "#396"

                Tree ->
                    "#274"

                Town ->
                    "#765"

                Village ->
                    "#c97"

                Water ->
                    "#49f"
    in
    Svg.svg
        [ Attr.x (String.fromInt x)
        , Attr.y
            (String.fromFloat
                (if modBy 2 realX == 0 then
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
