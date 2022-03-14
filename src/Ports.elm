port module Ports exposing (playNote, playSong)

import Json.Encode
import MidiEvent
import Music
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
        noteToJson : Music.NoteEvent -> Json.Encode.Value
        noteToJson noteEvent =
            let
                midiEvent : MidiEvent.MidiEvent
                midiEvent =
                    MidiEvent.fromNoteEvent noteEvent
            in
            Json.Encode.object
                [ ( "midi", Json.Encode.int midiEvent.pitch )
                , ( "index", Json.Encode.int midiEvent.start )
                ]
    in
    outgoing
        { tag = "playSong"
        , data =
            Song.notes song
                |> Json.Encode.list noteToJson
        }
