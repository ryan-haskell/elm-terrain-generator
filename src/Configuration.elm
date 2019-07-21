module Configuration exposing (Configuration, Settings, parse)

import Json.Decode as D exposing (Decoder)


type alias Configuration =
    { seed : Int
    , size : Int
    , water : Settings
    , trees : Settings
    , towns : Settings
    , villages : Settings
    }


type alias Settings =
    { count : Int
    , depth : Int
    , chance : Int
    }


decoder : Decoder Configuration
decoder =
    D.map6 Configuration
        (D.field "seed" D.int)
        (D.field "size" D.int)
        (D.field "water" settingsDecoder)
        (D.field "trees" settingsDecoder)
        (D.field "towns" settingsDecoder)
        (D.field "villages" settingsDecoder)


settingsDecoder : Decoder Settings
settingsDecoder =
    D.map3 Settings
        (D.field "count" D.int)
        (D.field "depth" D.int)
        (D.field "chance" D.int)


parse : String -> Maybe Configuration
parse json =
    D.decodeString decoder json
        |> Result.toMaybe
