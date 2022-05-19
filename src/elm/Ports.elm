port module Ports exposing (playNote, playSong)

import Json.Encode
import Music.Duration
import Music.Event as Event
import Music.Note as Note
import Music.Pitch
import Music.Tempo as Tempo
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
        beatsPerMinute : Float
        beatsPerMinute =
            Project.tempo project
                |> Tempo.toSerial
                |> .beatsPerMinute
                |> Basics.toFloat

        bpmAtWhichQuarterNotesLastOneSecond : Float
        bpmAtWhichQuarterNotesLastOneSecond =
            60

        durationInRealTime : Float -> Float
        durationInRealTime duration =
            duration * (bpmAtWhichQuarterNotesLastOneSecond / beatsPerMinute) * 4

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
                        |> durationInRealTime

                noteDuration : Float
                noteDuration =
                    Note.duration noteEvent.value
                        |> Music.Duration.toFloat
                        |> durationInRealTime
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
