module File.Save exposing (save)

import File.Download
import File.Version.V0
import Json.Encode
import Project


save : String -> Project.Project -> Cmd msg
save fileName project =
    Json.Encode.encode 0 (encode project)
        |> File.Download.string (fileName ++ ".shidi") "text/shidi"


encode : Project.Project -> Json.Encode.Value
encode project =
    File.Version.V0.encode project
