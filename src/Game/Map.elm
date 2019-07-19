module Game.Map exposing
    ( Map
    , Tile(..)
    , get
    , init
    )

import Grid exposing (Grid)
import Random as R


type Map
    = Map Int (Grid ( Int, Int ) Tile)


type Tile
    = Grass
    | Water Int
    | Tree Int


init : { depth : Int, size : Int, seed : Int, chance : Float } -> Map
init { depth, size, seed, chance } =
    let
        plant =
            Grid.plant
                (neighbors size)
                { depth = depth, chance = chance }
    in
    Map size
        (Grid.init
            seed
            Grass
            (coordinateGenerator size)
            |> plant Water
            |> plant Water
            |> plant Water
            |> plant Water
            |> plant Tree
            |> plant Tree
            |> plant Tree
        )


coordinateGenerator : Int -> R.Generator ( Int, Int )
coordinateGenerator size =
    R.map2 (\x y -> ( x, y ))
        (R.int 0 size)
        (R.int 0 size)


neighbors : Int -> ( Int, Int ) -> List ( Int, Int )
neighbors size ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y - 1 )
    , ( x - 1, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y + 1 )
    ]
        |> List.map (Tuple.mapBoth (modBy size) (modBy size))


get : ( Int, Int ) -> Map -> Tile
get ( x, y ) (Map _ grid) =
    Grid.get ( x, y ) grid
