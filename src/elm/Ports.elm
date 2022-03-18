port module Ports exposing (playNote, playSong)

import Json.Encode
import Music
import Music.Duration
import Music.Note
import Music.Pitch
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
                midiNoteNumber : Int
                midiNoteNumber =
                    Music.Note.pitch noteEvent.value
                        |> Music.Pitch.toMIDINoteNumber

                noteStartTime : Int
                noteStartTime =
                    Music.Duration.toFloat noteEvent.at
                        * 4
                        |> Basics.round
            in
            Json.Encode.object
                [ ( "midi", Json.Encode.int midiNoteNumber )
                , ( "index", Json.Encode.int noteStartTime )
                ]
    in
    outgoing
        { tag = "playSong"
        , data =
            Project.noteEvents project
                |> Json.Encode.list noteToJson
        }
