module Lucky.Settings exposing (Settings)

import Difficulty exposing (Difficulty)


type alias Settings =
    { difficulty : Difficulty
    , playerName : String
    , morePlayerNames : List String
    }
