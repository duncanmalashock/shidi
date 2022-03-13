module File.Version0 exposing (decoder, encode)

import Coordinate
import Json.Decode
import Json.Encode
import Song exposing (Song)


encode : Song -> Json.Encode.Value
encode song =
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


decoder : Json.Decode.Decoder Song
decoder =
    notesDecoder
        |> Json.Decode.map Song.new


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
