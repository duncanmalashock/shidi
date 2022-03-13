port module Ports exposing (playNote, playSong)

import Coordinate
import Json.Encode
import Song


port outgoing :
    { tag : String
    , data : Json.Encode.Value
    }
    -> Cmd msg


playNote : Int -> Cmd msg
playNote noteNumber =
    outgoing
        { tag = "playNote"
        , data = Json.Encode.int noteNumber
        }


playSong : Song.Song -> Cmd msg
playSong song =
    let
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
    in
    outgoing
        { tag = "playSong"
        , data =
            Song.notes song
                |> Json.Encode.list noteToJson
        }
