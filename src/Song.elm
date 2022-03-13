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
    = Song (AssocSet.Set Coordinate.GridCells)


new : Song
new =
    Song AssocSet.empty


addNote : Coordinate.GridCells -> Song -> Song
addNote newNote (Song song) =
    Song (AssocSet.insert newNote song)


removeNote : Coordinate.GridCells -> Song -> Song
removeNote newNote (Song song) =
    Song (AssocSet.remove newNote song)


toList : Song -> List Coordinate.GridCells
toList (Song song) =
    AssocSet.toList song


toJson : Song -> Json.Encode.Value
toJson (Song song) =
    Json.Encode.list noteToJson (AssocSet.toList song)


noteToJson : Coordinate.GridCells -> Json.Encode.Value
noteToJson note =
    let
        { x, y } =
            Coordinate.toGridCellsRecord note
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


decoder : Json.Decode.Decoder (AssocSet.Set Coordinate.GridCells)
decoder =
    noteDecoder
        |> Json.Decode.list
        |> Json.Decode.map AssocSet.fromList


noteDecoder : Json.Decode.Decoder Coordinate.GridCells
noteDecoder =
    Json.Decode.map2 Coordinate.gridCells
        (Json.Decode.field "index" Json.Decode.int)
        (Json.Decode.field "midi" Json.Decode.int
            |> Json.Decode.map toGridRowIndexFromMidiNote
        )
