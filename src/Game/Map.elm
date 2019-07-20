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
    | Water
    | Tree


type alias Options =
    { seed : Int
    , size : Int
    , tile : Tile
    , tiles : List PlantOptions
    }


type alias PlantOptions =
    { count : Int
    , tile : Int -> Tile
    , depth : Int
    , chance : Float
    }


init : Options -> Map
init { seed, size, tile, tiles } =
    let
        plant : PlantOptions -> Grid ( Int, Int ) Tile -> Grid ( Int, Int ) Tile
        plant options grid =
            if options.count > 0 then
                plant
                    { options | count = options.count - 1 }
                    (Grid.plant
                        (neighbors size)
                        { depth = options.depth, chance = options.chance }
                        options.tile
                        grid
                    )

            else
                grid
    in
    Map size
        (List.foldl
            plant
            (Grid.init
                seed
                tile
                (coordinateGenerator size)
            )
            tiles
        )


coordinateGenerator : Int -> R.Generator ( Int, Int )
coordinateGenerator size =
    R.map2 (\x y -> ( x, y ))
        (R.int 0 size)
        (R.int 0 size)


neighbors : Int -> ( Int, Int ) -> List ( Int, Int )
neighbors size ( x, y ) =
    [ ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]
        |> List.map (Tuple.mapBoth (modBy size) (modBy size))


get : ( Int, Int ) -> Map -> Tile
get ( x, y ) (Map _ grid) =
    Grid.get ( x, y ) grid
