module File.Load exposing (load)

import File
import File.Version.Version0
import Json.Decode
import Song
import Task


load :
    (Result Json.Decode.Error Song.Song -> msg)
    -> File.File
    -> Cmd msg
load toMsg file =
    Task.perform toMsg
        (File.toString file
            |> Task.map decode
        )


decode : String -> Result Json.Decode.Error Song.Song
decode jsonString =
    Json.Decode.decodeString
        (Json.Decode.oneOf decoders)
        jsonString


decoders : List (Json.Decode.Decoder Song.Song)
decoders =
    [ File.Version.Version0.decoder
    ]
