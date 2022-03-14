module File.Save exposing (save)

import File.Download
import File.Version.V0
import Json.Encode
import Song


save : String -> Song.Song -> Cmd msg
save fileName song =
    Json.Encode.encode 0 (encode song)
        |> File.Download.string (fileName ++ ".shidi") "text/shidi"


encode : Song.Song -> Json.Encode.Value
encode song =
    File.Version.V0.encode song
