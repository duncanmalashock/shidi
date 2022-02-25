port module Ports exposing (playNote, playSong)

import Json.Encode


port outgoing :
    { tag : String
    , data : Json.Encode.Value
    }
    -> Cmd msg


playNote : Int -> Cmd msg
playNote noteNumber =
    outgoing
        { tag = "playNote"
        , data = Json.Encode.int noteNumber
        }


playSong : Json.Encode.Value -> Cmd msg
playSong notes =
    outgoing
        { tag = "playSong"
        , data = notes
        }
