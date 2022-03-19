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

                noteStart : Float
                noteStart =
                    noteEvent.at
                        |> Music.Duration.toFloat
                        |> Debug.log "noteStart"

                noteDuration : Float
                noteDuration =
                    Music.Note.duration noteEvent.value
                        |> Music.Duration.toFloat
                        |> Debug.log "noteDuration"
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
