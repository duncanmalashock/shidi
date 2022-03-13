module File.Version exposing (Version(..), current, decoders, toString)

import File.Version0
import Json.Decode
import Song


type Version
    = Version0


toString : Version -> String
toString version =
    case version of
        Version0 ->
            "0"


current : Version
current =
    Version0


decoders : List (Json.Decode.Decoder Song.Song)
decoders =
    [ File.Version0.decoder
    ]
