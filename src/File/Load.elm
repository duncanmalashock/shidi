module File.Load exposing (load)

import File
import File.Version.V0
import Json.Decode
import Project
import Task


load :
    (Result Json.Decode.Error Project.Project -> msg)
    -> File.File
    -> Cmd msg
load toMsg file =
    Task.perform toMsg
        (File.toString file
            |> Task.map decode
        )


decode : String -> Result Json.Decode.Error Project.Project
decode jsonString =
    Json.Decode.decodeString
        (Json.Decode.oneOf decoders)
        jsonString


decoders : List (Json.Decode.Decoder Project.Project)
decoders =
    [ File.Version.V0.decoder
    ]
