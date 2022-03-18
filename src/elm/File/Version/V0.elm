module File.Version.V0 exposing (decoder, encode)

import Json.Decode
import Json.Encode
import Music
import Music.Duration
import Music.Note
import Music.Pitch
import Project


encode : Project.Project -> Json.Encode.Value
encode project =
    Json.Encode.list noteToJson (Project.noteEvents project)


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


decoder : Json.Decode.Decoder Project.Project
decoder =
    notesDecoder
        |> Json.Decode.map Project.new


notesDecoder : Json.Decode.Decoder (List Music.NoteEvent)
notesDecoder =
    Json.Decode.list noteDecoder


noteDecoder : Json.Decode.Decoder Music.NoteEvent
noteDecoder =
    let
        toNoteEvent : { pitch : Int, start : Int } -> Music.NoteEvent
        toNoteEvent { pitch, start } =
            { at = Music.Duration.multiplyByInt start Music.Duration.quarter
            , value =
                Music.Note.note (Music.Pitch.fromMIDINoteNumber pitch)
                    Music.Duration.quarter
            }
    in
    Json.Decode.map2
        (\start pitch ->
            { start = start
            , pitch = pitch
            }
                |> toNoteEvent
        )
        (Json.Decode.field "index" Json.Decode.int)
        (Json.Decode.field "midi" Json.Decode.int)
