module File.Version.V0 exposing (decoder, encode)

import Json.Decode
import Json.Encode
import Music
import Music.Duration
import Music.Event as Event
import Music.Note as Note
import Music.Pitch
import Music.Tempo as Tempo
import Project


encode : Project.Project -> Json.Encode.Value
encode project =
    Json.Encode.list noteToJson (Project.noteEvents project)


noteToJson : Event.Event Note.Note -> Json.Encode.Value
noteToJson noteEvent =
    let
        midiNoteNumber : Int
        midiNoteNumber =
            Note.pitch noteEvent.value
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
        |> Json.Decode.map
            (\notes ->
                Project.new
                    { noteEvents = notes
                    , tempo = Tempo.quarterNotesPerMinute 120
                    }
            )


notesDecoder : Json.Decode.Decoder (List (Event.Event Note.Note))
notesDecoder =
    Json.Decode.list noteDecoder


noteDecoder : Json.Decode.Decoder (Event.Event Note.Note)
noteDecoder =
    let
        toNoteEvent : { pitch : Int, start : Int } -> Event.Event Note.Note
        toNoteEvent { pitch, start } =
            { at = Music.Duration.multiplyByInt start Music.Duration.quarter
            , value =
                Note.note (Music.Pitch.fromMIDINoteNumber pitch)
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
