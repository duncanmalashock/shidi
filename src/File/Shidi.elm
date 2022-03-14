module File.Shidi exposing (load, save)

import File
import File.Download
import File.Version
import File.Version0
import Json.Decode
import Json.Encode
import Song
import Task


save : String -> Song.Song -> Cmd msg
save fileName song =
    Json.Encode.encode 0 (encode song)
        |> File.Download.string (fileName ++ ".shidi") "text/shidi"


load :
    (Result Json.Decode.Error Song.Song -> msg)
    -> File.File
    -> Cmd msg
load toMsg file =
    Task.perform toMsg
        (File.toString file
            |> Task.map decode
        )


encode : Song.Song -> Json.Encode.Value
encode song =
    case File.Version.current of
        File.Version.Version0 ->
            File.Version0.encode song


decode : String -> Result Json.Decode.Error Song.Song
decode jsonString =
    Json.Decode.decodeString
        (Json.Decode.oneOf File.Version.decoders)
        jsonString
