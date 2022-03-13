module Song exposing
    ( Song, new, fromJsonString
    , addNote, removeNote
    , toJson, toList
    )

{-|

@docs Song, new, fromJsonString

@docs addNote, removeNote

@docs toJson, toList

-}

import AssocSet
import Coordinate
import Json.Decode
import Json.Encode


type Song
    = Song (AssocSet.Set Coordinate.PianoRoll)


new : Song
new =
    Song AssocSet.empty


addNote : Coordinate.PianoRoll -> Song -> Song
addNote newNote (Song song) =
    Song (AssocSet.insert newNote song)


removeNote : Coordinate.PianoRoll -> Song -> Song
removeNote newNote (Song song) =
    Song (AssocSet.remove newNote song)


toList : Song -> List Coordinate.PianoRoll
toList (Song song) =
    AssocSet.toList song


toJson : Song -> Json.Encode.Value
toJson (Song song) =
    Json.Encode.list noteToJson (AssocSet.toList song)


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


toGridRowIndexFromMidiNote : Int -> Int
toGridRowIndexFromMidiNote input =
    131 - input


fromJsonString : String -> Result Json.Decode.Error Song
fromJsonString jsonString =
    case Json.Decode.decodeString decoder jsonString of
        Ok song ->
            Ok (Song song)

        Err error ->
            Err error


decoder : Json.Decode.Decoder (AssocSet.Set Coordinate.PianoRoll)
decoder =
    noteDecoder
        |> Json.Decode.list
        |> Json.Decode.map AssocSet.fromList


noteDecoder : Json.Decode.Decoder Coordinate.PianoRoll
noteDecoder =
    Json.Decode.map2 Coordinate.pianoRoll
        (Json.Decode.field "index" Json.Decode.int)
        (Json.Decode.field "midi" Json.Decode.int
            |> Json.Decode.map toGridRowIndexFromMidiNote
        )
