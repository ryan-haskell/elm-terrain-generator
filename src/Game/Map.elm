module Game.Map exposing
    ( Map
    , Tile(..)
    , get
    , init
    , neighbors
    )

import Grid exposing (Grid)
import Random as R


type Map
    = Map Int (Grid ( Int, Int ) Tile)


type Tile
    = Grass
    | Water
    | Tree
    | Village
    | Town


type alias Options =
    { seed : Int
    , size : Int
    , tile : Tile
    , tiles : List PlantOptions
    }


type alias PlantOptions =
    { count : Int
    , tile : Tile
    , depth : Int
    , chance : Float
    }


init : Options -> Map
init { seed, size, tile, tiles } =
    Map size
        (List.foldl
            (plant size)
            (Grid.init
                seed
                tile
                (coordinateGenerator size)
            )
            tiles
        )


plant : Int -> PlantOptions -> Grid ( Int, Int ) Tile -> Grid ( Int, Int ) Tile
plant size options grid =
    if options.count > 0 then
        plant
            size
            { options | count = options.count - 1 }
            (Grid.plant
                (neighbors size)
                { depth = options.depth, chance = options.chance }
                (always options.tile)
                grid
            )

    else
        grid


coordinateGenerator : Int -> R.Generator ( Int, Int )
coordinateGenerator size =
    R.map2 (\x y -> ( x, y ))
        (R.int 0 size)
        (R.int 0 size)


neighbors : Int -> ( Int, Int ) -> List ( Int, Int )
neighbors size ( x, y ) =
    let
        operator =
            if modBy 2 x == 1 then
                (+)

            else
                (-)
    in
    [ ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y )
    , ( x - 1, operator y 1 )
    , ( x + 1, y )
    , ( x + 1, operator y 1 )
    ]
        |> List.map (Tuple.mapBoth (modBy size) (modBy size))


get : ( Int, Int ) -> Map -> Tile
get ( x, y ) (Map _ grid) =
    Grid.get ( x, y ) grid
