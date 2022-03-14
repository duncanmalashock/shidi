port module Ports exposing (playNote, playSong)

import Json.Encode
import MidiEvent
import Music
import Project


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


playSong : Project.Project -> Cmd msg
playSong project =
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
            Project.noteEvents project
                |> Json.Encode.list noteToJson
        }
