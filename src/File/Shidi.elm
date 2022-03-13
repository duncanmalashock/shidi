module File.Shidi exposing (load, save)

import Coordinate
import File
import File.Download
import Json.Decode
import Json.Encode
import Song exposing (Song)
import Task


save : String -> Song.Song -> Cmd msg
save fileName song =
    Json.Encode.encode 0 (songToJson song)
        |> File.Download.string (fileName ++ ".shidi") "text/shidi"


songToJson : Song -> Json.Encode.Value
songToJson song =
    Json.Encode.list noteToJson (Song.notes song)


noteToJson : Coordinate.PianoRoll -> Json.Encode.Value
noteToJson note =
    let
        { x, y } =
            Coordinate.toPianoRollRecord note
    in
    Json.Encode.object
        [ ( "midi", Json.Encode.int (fromGridRowIndexToMidiNote y) )
        , ( "index", Json.Encode.int x )
        ]


fromGridRowIndexToMidiNote : Int -> Int
fromGridRowIndexToMidiNote input =
    131 - input


load :
    (Result Json.Decode.Error Song -> msg)
    -> File.File
    -> Cmd msg
load toMsg file =
    Task.perform toMsg
        (File.toString file
            |> Task.map jsonStringToSong
        )


jsonStringToSong : String -> Result Json.Decode.Error Song
jsonStringToSong jsonString =
    case Json.Decode.decodeString notesDecoder jsonString of
        Ok notes ->
            Ok (Song.new notes)

        Err error ->
            Err error


notesDecoder : Json.Decode.Decoder (List Coordinate.PianoRoll)
notesDecoder =
    Json.Decode.list noteDecoder


noteDecoder : Json.Decode.Decoder Coordinate.PianoRoll
noteDecoder =
    Json.Decode.map2 Coordinate.pianoRoll
        (Json.Decode.field "index" Json.Decode.int)
        (Json.Decode.field "midi" Json.Decode.int
            |> Json.Decode.map toGridRowIndexFromMidiNote
        )


toGridRowIndexFromMidiNote : Int -> Int
toGridRowIndexFromMidiNote input =
    131 - input
