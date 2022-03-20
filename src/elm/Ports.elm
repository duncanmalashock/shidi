port module Ports exposing (playNote, playSong)

import Json.Encode
import Music
import Music.Duration
import Music.Event as Event
import Music.Note as Note
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
        noteToJson : Event.Event Note.Note -> Json.Encode.Value
        noteToJson noteEvent =
            let
                midiNoteNumber : Int
                midiNoteNumber =
                    Note.pitch noteEvent.value
                        |> Music.Pitch.toMIDINoteNumber

                noteStart : Float
                noteStart =
                    noteEvent.at
                        |> Music.Duration.toFloat

                noteDuration : Float
                noteDuration =
                    Note.duration noteEvent.value
                        |> Music.Duration.toFloat
            in
            Json.Encode.object
                [ ( "midi", Json.Encode.int midiNoteNumber )
                , ( "start", Json.Encode.float noteStart )
                , ( "duration", Json.Encode.float noteDuration )
                ]
    in
    outgoing
        { tag = "playSong"
        , data =
            Project.noteEvents project
                |> Json.Encode.list noteToJson
        }
